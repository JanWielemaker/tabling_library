for f in $*; do
  swipl -g test,halt autotest.pl $f
done
