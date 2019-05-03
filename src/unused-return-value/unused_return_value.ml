open Core_kernel
open Bap.Std
open Bap_primus.Std

include Self ()

let unused = Primus.Machine.State.declare
    ~name:"subroutines-with-unused-result"
    ~uuid:"af66d451-fb62-44c3-9c2a-8969e111ad91"
    (fun _ -> Map.empty (module Addr))

type stacks = {
    stacks : Primus.value list Primus.Value.Map.t;
}

let stack = Primus.Machine.State.declare
    ~name:"lisp-stack"
    ~uuid:"ce3ba966-ba5f-4e8b-9851-f733b938d24c"
    (fun _ -> {stacks = Primus.Value.Map.empty})

module Notify(Machine : Primus.Machine.S) = struct
  module Value = Primus.Value.Make(Machine)
  open Machine.Syntax

  [@@@warning "-P"]
  let run [name; addr] =
    Value.Symbol.of_value name >>= fun name ->
    let addr = Value.to_word addr in
    if Addr.is_zero addr then Value.b1
    else
      Machine.Global.get unused >>= fun s ->
      if not @@ Map.mem s addr then
        printf "Detected unused result from function %s at %s\n" name
          (Addr.to_string addr);
      Machine.Global.put unused (Map.set s addr name) >>= fun () ->
      Value.b1

end

module Output_arg(Machine : Primus.Machine.S) = struct
  module Value = Primus.Value.Make(Machine)
  module Env = Primus.Env.Make(Machine)
  open Machine.Syntax

  let find sub =
    Machine.get () >>= fun proj ->
    Value.Symbol.of_value sub >>= fun name ->
    let subs = Term.to_sequence sub_t (Project.program proj) in
    match Seq.find subs ~f:(fun s -> String.(Sub.name s = name)) with
    | None -> Machine.return None
    | Some sub ->
       Seq.find (Term.to_sequence arg_t sub)
         ~f:(fun a -> Arg.intent a = Some Out) |> function
    | None -> Machine.return None
    | Some out -> Machine.return (Some out)

end

module Return_val(Machine : Primus.Machine.S) = struct
  module Value = Primus.Value.Make(Machine)
  module Env = Primus.Env.Make(Machine)
  module Out = Output_arg(Machine)
  open Machine.Syntax

  [@@@warning "-P"]
  let run [sub] =
    Out.find sub >>= function
    | None -> Value.b0
    | Some a -> Env.get (Arg.lhs a)
end

module Return_var(Machine : Primus.Machine.S) = struct
  module Value = Primus.Value.Make(Machine)
  module Env = Primus.Env.Make(Machine)
  module Out = Output_arg(Machine)
  open Machine.Syntax

  [@@@warning "-P"]
  let run [sub] =
    Out.find sub >>= function
    | None -> Value.b0
    | Some a -> Value.Symbol.to_value (Var.name (Arg.lhs a))
end


module Stack(Machine : Primus.Machine.S) = struct
  module Value = Primus.Value.Make(Machine)
  open Machine.Syntax

  let push name x =
    Machine.Local.update stack ~f:(fun s -> {
        stacks =
          Map.update s.stacks name ~f:(function
            | None -> [x]
            | Some xs -> x :: xs)}) >>= fun () ->
    Value.b1

  let pop name =
    Machine.Local.get stack >>= fun s ->
    match Map.find s.stacks name with
    | None | Some [] -> Value.b0
    | Some (x :: xs) ->
       let s = {stacks = Map.set s.stacks name xs} in
       Machine.Local.put stack s >>= fun () ->
       Machine.return x
end

module Push(Machine : Primus.Machine.S) = struct
  module Stack = Stack(Machine)
  [@@@warning "-P"]
  let run [name; x] = Stack.push name x
end

module Pop(Machine : Primus.Machine.S) = struct
  module Stack = Stack(Machine)
  [@@@warning "-P"]
  let run [name] = Stack.pop name
end

module Interface(Machine : Primus.Machine.S) = struct
  module Lisp = Primus.Lisp.Make(Machine)
  open Primus.Lisp.Type.Spec

  let init () =
    Machine.sequence [

      Lisp.define "notify-unused-result" (module Notify)
        ~types:(tuple [a; b] @-> c)
        ~docs:{|(notify-unused-result name addr) outputs result of analysis|};

      Lisp.define "return-arg" (module Return_var)
        ~types:(tuple [a] @-> b)
        ~docs:
        ({|(return-arg SUB) returns the name of output argument of the
           subroutine SUB. Returns NIL if the subroutine SUB doesn't
           return anything or subroutine's api is unknown|});

      Lisp.define "stack-push" (module Push)
        ~types:(tuple [a; b] @-> c)
        ~docs:({|(stack-push STACK X) pushes value X on the top of the stack STACK|});

      Lisp.define "stack-pop" (module Pop)
        ~types:(tuple [a] @-> b)
        ~docs:({|(stack-pop STACK) removes value from the top of the stack STACK and returns
                 it. Returns NIL if it was nothing in STACK.|});

      ]
end

let () = Config.when_ready (fun {Config.get=(!)} ->
             Primus.Machine.add_component (module Interface))
