### Description

How should the state borders of the United States be drawn?  This project
explores multiple strategies for drawing state borders to achieve a specific
goal and generates an interactive tool to generate state border maps based
on blends of those strategies.

### Repository Layout

This repository contains 3-4 directories.  For convenience, each directory name
has a unique letter prefix, such as "X_".

+ **A_Inputs** - Holds all source data files.  For coding purposes, I treat this
directory as read only.
+ **B_Intermediates** – Holds data files created as byproducts of the
computations performed.  This can include cached API pulls, temporary data
caches for large computations, and data objects to be passed from one script to
the next.
+ **C_Outputs** – Holds the final products for the project.
+ **D_Misc** – If needed, this directory will contain any other useful materials
for the project, such as source data documentation.

The scripts containing executable code are in the top level of the repository.
Each script has a unique numeric prefix, such as "2_", which indicates the order
in which to execute the scripts.

### Scripts (Actions, Inputs, and Outputs)

### Project Status and To-Dos

I am actively building the code for this project.  Basic data shaping is done;
now testing out border generation methods. Remaining to-dos include:

- [ ] Complete README Description section
- [ ] Complete README Scripts section
- [ ] Write analysis script
- [ ] Write static visualization script
- [ ] Write dynamic visualization script
- [ ] Build gallery entry
- [ ] Make sure gallery, README, and tool text are aligned
- [ ] Polish writing and grammar in all three sets of text
