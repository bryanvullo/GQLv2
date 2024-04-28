'auto' is a bash script. 
You can run it on uglogin.ecs.soton.ac.uk if you do not use a *nix system. 

To use:
Put your compiled interpreter executable called “Gql” in the automarker directory.
Put your solution prN.gql in the subdirectory prN/  for N = 1 to 5
Run ./auto whilst in the automarker directory.

To add further tests:
For each new test, put the input tile files in a subdirectory called inputN in subdirectory prN/inputs/
And put the expected output data in a file called expN.tl in subdirectory prN/expected/

Note:
The correctness check on the output is only a crude approximation of the actual correctness check I will use for testing and marking.  In this script I textually compare a sorted version of the output with all spaces removed.  For testing and marking proper I will parse your outputs and compare your output graphs for equality against the expected graphs. 
