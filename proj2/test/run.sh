#!/bin/bash

GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
BOLD='\033[1m'
NC='\033[0m'

TMPFILE="./tmp_output"
N_TESTS=20
PASS=0

echo -e "${BOLD}${BLUE}============================${NC}"
echo -e "${BOLD}${BLUE}    TURING MACHINE TESTS    ${NC}"
echo -e "${BOLD}${BLUE}============================${NC}"
echo ""

for i in $(seq 1 $N_TESTS); do
    printf "${BOLD}Test %2d:${NC} " $i
    ../flp24-log < "inputs/in$i.txt" > "$TMPFILE" 2>/dev/null
    
    if diff -q "expected/exp$i.txt" "$TMPFILE" > /dev/null; then
        echo -e "${GREEN}[PASS]${NC}"
        PASS=$((PASS + 1))
    else
        echo -e "${RED}[FAIL]${NC}"
        echo -e "${YELLOW}Expected output:${NC}"
        echo -e "------------------------"
        cat "exp$i.txt"
        echo -e "------------------------"
        echo -e "${YELLOW}Actual output:${NC}"
        echo -e "------------------------"
        cat "$TMPFILE"
        echo ""
    fi
done

rm -f "$TMPFILE"

FAIL=$((N_TESTS - PASS))
PERCENTAGE=$((PASS * 100 / N_TESTS))

echo -e "${BLUE}============================${NC}"
echo -e "${BOLD}SUMMARY:${NC}"
echo -e "  ${GREEN}PASS:${NC} $PASS/$N_TESTS (${PERCENTAGE}%)"
if [ $FAIL -gt 0 ]; then
    echo -e "  ${RED}FAIL:${NC} $FAIL/$N_TESTS"
else
    echo -e "  ${GREEN}FAIL:${NC} $FAIL/$N_TESTS"
fi
echo -e "${BLUE}============================${NC}"
