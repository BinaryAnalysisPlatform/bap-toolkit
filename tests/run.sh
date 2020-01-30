#!/usr/bin/env sh

XFAILS="warn-unused must-check-value"
# Reasons:
# 1) Taint analysis Garbage collector need to be fixed:
#    the finished observation never reached it, need use halting.
#    Checks warn-unused must-check-value are rely on it.
# 2) Also, warn-unused may fail because we need to fix attributes on
#    in callsites plugin, since they are not tranfered from the original
#    argument.
#

logfile="toolkit.log"
litmuses="litmus-tests"
status=OK

update_status() {
    if [ ! "is$status" = "isFAIL" ]; then
        result=`echo $1 | grep -v XFAIL | grep FAIL`
        if [ ! "OK$result" = "OK" ]; then
            status=FAIL
        fi
    fi
}

run_bap() {
    name=$1
    binary=$2
    recipe=$3
    api_path=$4
    start=`date | cut -d' ' -f4`
    echo $name: $start bap $binary --recipe=$recipe $api_path >> $logfile
    bap $binary --recipe=$recipe $api_path > /dev/null 2> /dev/null
    finish=`date | cut -d' ' -f4`
    echo "$finish finished" >> $logfile
}

compare() {
    name=$1
    expected_incidents=$2
    exact=$3

    expected_fail=""

    for c in $XFAILS; do
        if [ "expected$c" = "expected$name" ]; then
            expected_fail="--expect-fail"
        fi
    done

    result=`./compare-incidents $name $expected_incidents incidents $exact $expected_fail`
    echo "" >> $logfile
    echo $result

    update_status "$result"
}


litmuses_run() {
    echo "                             LITMUS TESTS"

    for name in `ls litmus-tests/bin`; do

        binary=$litmuses/bin/$name
        data=$litmuses/data/$name
        expected_incidents=$data/expected

        if [ -d $data/api ]; then
            api="--api-path=$data/api"
        else
            api=""
        fi

        run_bap $name $binary $name $api
        compare $name $expected_incidents "--exact"

        rm -f incidents

    done
}

file_not_found() {
    bug="ERROR: $1 not found"
    echo $bug >> $logfile
    echo ""   >> $logfile
    echo $bug
}


artifacts_run() {
    echo "                         ARTIFACTS (patience!)"
    dir=artifacts

    for arti in `ls $dir`; do
        for check in `ls $dir/$arti | grep -v artifact`; do
            artifact=$dir/$arti/artifact

            if [ ! -f $artifact ]; then
                file_not_found $artifact
                continue
            fi

            expected_incidents=$dir/$arti/$check/expected
            run=$dir/$arti/$check/run
            api=$dir/$arti/$check/api

            if [ ! -f $run ]; then
                file_not_found $run
                continue
            fi

            api_path=
            if [ -d $api ]; then
                api_path="--api-path=$api"
            fi

            recipe=`cat $run`

            cve=`echo $check | grep CVE`
            if [ "no$cve" = "no" ]; then
                name=$arti/$check
            else
                name=$check
            fi

            run_bap $name $artifact $recipe $api_path
            compare $name $expected_incidents

            rm -f incidents
        done
    done
}

rm -rf log
rm -f toolkit.log

start=`date | cut -d' ' -f4`
echo "started at $start" >> toolkit.log
litmuses_run
echo ""
artifacts_run
echo ""

if [ "is$status" = "isFAIL" ]; then
    echo "Some tests FAILED"
    exit 1
fi

echo "OK"
