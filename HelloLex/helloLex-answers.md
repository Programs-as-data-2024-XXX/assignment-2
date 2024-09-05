## Question 1

The Regular expression in hello.fsl is "['0'-'9']" it represents the semantic values: 0, 1, 2, 3, 4, 5, 6, 7, 8, and 9, i.e. all single digit integers.

## Question 2
* The additional file generated is the file hello.fs 
* There are 3 states.

## Question 3
* We have compiled and ran the hello.fs genered in question 2

## Question 4
See hello2.fsl file if needed, but regex has been updated to "['0'-'9']+"

## Question 5
See hello3.fsl file, the regex used is "['+''-']?(['0'-'9']*['.'])?['0'-'9']+"

## Question 6
"34" is recognized by the regex, since it first specifies that there can be either be a single "+" or "-" in the front of the given input or none at all. Then it is specified that there can (but it does not have to) be 0 or more digits from the range of 0-9 followed by a period. Finally the regex specifies that there has to be 1 or more digits from the range of 0-9, this is why the inputs "34" and "34.24" is recognized completely, while the input "34,34" is only recognized as "34", since the regex does not recognize the comma, but only the period. Other inputs that would fail completely would be inputs such as "+-34" or "+"". Another input that would be accepted would be something like ".34" or "+34".
