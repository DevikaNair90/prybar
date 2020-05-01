## Test environments
* local Mac OS Darwin15 install, R 3.6.1
* CentOS 7.6, R 3.6.2

## R CMD check results
There were no ERRORs.

There was 1 WARNING:

* cheking whether package 'prybar' can be installed
 Warning: replacing previous import ‘data.table::dcast’ by ‘maditr::dcast’ when loading ‘prybar’
 Warning: replacing previous import ‘data.table::melt’ by ‘maditr::melt’ when loading ‘prybar’  
 
 This package doesn't require data.table, but upon removal, examples would no longer run, with an 
 error message to check whether object is a data.table. It seems related to these issues, but I 
 am not sure how to proceed with this. 
 
 * https://stackoverflow.com/questions/37866246/why-this-simple-test-with-data-table-fails-how-to-fix-it?noredirect=1&lq=1. 
 * https://stackoverflow.com/questions/27980835/r-data-table-works-in-direct-call-but-same-function-in-a-package-fails 


