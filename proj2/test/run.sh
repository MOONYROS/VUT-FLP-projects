#!/bin/bash
TMPFILE="./tmp_output"
N_TESTS=16
PASS=0

for i in $(seq 1 $N_TESTS); do
    echo "Running test $i..."
    
    ../flp24-log < "in$i.txt" > "$TMPFILE"
    if diff -q "exp$i.txt" "$TMPFILE" > /dev/null; then
        echo "Test $i passed"
        PASS=$((PASS + 1))
    else
        echo "Test $i failed"
        echo "Expected:"
        cat "exp$i.txt"
        echo "Got:"
        cat "$TMPFILE"
    fi
    echo
done

rm -f "$TMPFILE"

echo "PASSED: $PASS"
echo "FAILED: $((N_TESTS - PASS))"
