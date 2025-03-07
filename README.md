
This repository contains, except for the publicly available benchmarks, everything that should be needed to reproduce the results of the paper "Soft and Constrained Hypertree Width" [1].


## How to Run Experiments 

1) First Compile the Scala Query Rewriting library into a JAR file.

   Refer to the ReadMe in the folder "Scala Rewriting Tool" for details

2) Setup Benchmarks and Database.

   The experiments presented here only support PostgreSQL and we do not guarantee that experiments here can run on any other DMBS. The user needs to setup an installation of PostgreSQL, with databases of the three publicly available benchmarks, with the scaling factors indicated in the paper and the notebook in this repository. The user needs to recall the name of the databases, the user names and passwords, these will be needed for the JupyterLab notebook.

3) Run the JupyterLab Notebook.
   Given steps 1 and 2, the notebook should run without issues. The location for the JAR file, and for each experiment the config data (database name, user and password) needs to be provided. We refer to the comments in the notebook on how to run it.


## Reference
[1] _Soft and Constrained Hypertree Width_  
Matthias Lanzinger, Cem Okulmus, Reinhard Pichler, Alexander Selzer and Georg Gottlob  
Accepted by the ACM Symposium on Principles of Database Systems (PODS) 2025.  
Full paper: https://doi.org/10.48550/arXiv.2412.11669
