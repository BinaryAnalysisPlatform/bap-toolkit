
CHECKS = find-symbol null-ptr-deref unused-return-value

define apply
	@export status=0;\
		for check in $(CHECKS); do make -C $$check $(1) || status=1; done;\
	exit $$status
endef

build:
	$(call apply,build)

install:
	$(call apply,install)

uninstall:
	$(call apply,uninstall)

reinstall:
	$(call apply,reinstall)

clean:
	$(call apply,clean)
