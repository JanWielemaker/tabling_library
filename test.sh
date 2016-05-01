if [ -z "$1" ]; then
  tests="$(echo example*.pl)"
else
  tests="$*"
fi

for f in $tests; do
  echo "TEST $f ..."
  swipl -g test,halt autotest.pl $f
done
