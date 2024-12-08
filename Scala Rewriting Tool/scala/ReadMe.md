# Scala Rewriting Tool "QueryRewriter"

The rewriter presented here stems from work previously published by Daniela Böhm, in collaboration with Georg Gottlob, Matthias Lanzinger, Reinhard Pichler and Alexander Selzer. (https://repositum.tuwien.at/handle/20.500.12708/202246) 

It has been modified, primarily by extracting small parts of the query optimiser for Spark that was presented there and adding different ways to extract costs from a Postgres installation. 

## Compile out a .JAR file 

At the base of the Scala project files (where 'src' and 'lib' folders and the 'build.sbt' file is present) execute: 

	sbt assembly