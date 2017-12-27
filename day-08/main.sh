total="$(cat input.txt | tr -d '\n' | wc -c)"
inmem="$(sh unquote.sh | wc -c)"
echo "${total}-${inmem}" | bc
