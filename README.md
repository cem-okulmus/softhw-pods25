
This repository contains, except for the publicly available benchmarks, everything that should be needed to reproduce the results of the paper "Soft and Constrained Hypertree Width".


## How to Run Experiments 

1) First Compile the Scala Query Rewriting library into a JAR file.

   Refer to the ReadMe in the folder "Query Rewriting Tool" for details

2) Setup Benchmarks and Database.

   Currently, we only support PostgreSQL and do not guarantee that experiments here can run on other DMBS. The users needs to setup an installation of PostgreSQL, with databases of the three publicly available benchmarks, with the scaling factors indicated in the paper. The user needs to recall the name of the databases, the user names and passwords, these will be needed for the JupyterLab notebook.

3) Run the JupyterLab Notebook.
   Given steps 1 and 2, the notebook should run without issues. The location for the JAR file, and for each experiments the config data (database name, user and password) needs to be provided. We refer to the comments in the notebook on how to run it.
