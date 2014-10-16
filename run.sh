clear
echo "-----------------------COMPILING QUADTREE---------------------------"
cs3110 compile quadtree.ml &&
cs3110 compile quadtree_test.ml &&
echo "-------------------------RUNNING QUADTREE---------------------------" &&
cs3110 test quadtree_test.ml
