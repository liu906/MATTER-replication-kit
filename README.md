# MATTER
## _A consistent performance evaluation for defect prediction models_


MATTER, a fraMework towArd a consisTenT pErformance compaRison, 
which allows for discovering really useful defect prediction models 
and hence can help advance the state-of-the-art in a trustworthy way.
Our replication kit include: 
- MATTER framework
- implementation (by ourseleves and other researchers) of multiple representative defect prediction models   
- detail prediction results of representative defect prediction models (module level) on JURECZKO, AEEEM, NASA MDP, NETGENE, Relink
## Features

- Provide a baseline defect prediction model to provide "floor" performance to be compare with
- Two SQA-effort-alignment settings
- Novel effort-aware performance indicators for defect prediction model evaluation
- Super lightweigh since it is implemented as simple r script

## Enviroment

MATTER is implemented by R-4.0.5, we run it on our  Win10 64 bit machine and it works fine.

## How to use MATTER
For models to be evaluated under MATTER,
- First, use [experiment/getPerformanceUnderGivenSQAeffort.r][root] to get prediction performance under given avaviable SQA effort
- Then, run [run_ONE.r][root] to get the performance of baseline model ONE under the same avaviable SQA effort
- Next, use [summary_performance][root] to get the prediction performance of the studied models and baseline model ONE
- Last, we provide [experiment/statistic.r][root] to compare the evaluated models and baseline model ONE statistically.

## Prerequisites of MATTER
MATTER requires the file of detail prediction results (module level) as input.  
To evaluate the defect prediction models' performance, (1) the predicted rank of modules in the target file, (2) the SLOC of each module, and (3) the true label of each module. 
Specifically, the input file which contains the prediction results of prediction model to be evaluated should be like this:


| sloc | predictedValue | predictLabel | actualBugLabel
| ------ | ------ | ------ | ------ |
| 12 | 0.55 | 0 | 1
| 4 | 0.00 | 0 | 0
| 831 | 1.00 | 1 | 0
| 321 | 0.78 | 1 | 1  


**Note that predictLabel and actualBugLabel must be binary, 0 for clean, 1 for defective.**


[//]: # (These are reference links used in the body of this note and get stripped out when the markdown processor does its job. There is no need to format nicely because it shouldn't be seen. Thanks SO - http://stackoverflow.com/questions/4823468/store-comments-in-markdown-syntax)

   
   [root]: <https://github.com/liu906/MATTER-replication-kit/>
