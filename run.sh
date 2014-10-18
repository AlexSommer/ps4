clear
echo "-----------------------COMPILING MODULES---------------------------"
cs3110 compile mutability.ml &&
cs3110 compile iterator.ml &&
cs3110 compile iterator_test.ml &&
echo "-------------------------RUNNING TEST SUITE---------------------------" &&
cs3110 test iterator_test.ml
