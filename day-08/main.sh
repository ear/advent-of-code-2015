total="$(cat input.txt | tr -d '\n' | wc -c)"
inmem="$(sh unquote.sh | wc -c)"
echo "${total}-${inmem}" | bc
total2="$(sh quote.sh | tr -d '\n' | wc -c)"
echo "${total2}-${total}" | bc
