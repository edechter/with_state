
all:


.PHONE: test
test:
	swipl -q -f t/test_with_state.pl -t 'halt(0)'
