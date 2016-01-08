/*
Authors: Pranav Kulkarni, Vivekanand Ramakrishnan, Sudha Manchukonda
Unity Ids - pkulkar5, vramakr2, smanchu
*/

Data set link - https://drive.google.com/open?id=0B4d0NY9oHJFvYmh3RTJ3OGhSaVk

Functionalities implemented:
----------------------------
1) Preprocessing - preprocessing.R file
2) SVM - svm.R
3) HMM - hmm.R (this file is to build HMM using user data set without considering section wise split (ability))
4) HMM-trackwise - hmmtrackwise.R (this file is to build HMM using user data set considering section wise split (ability))
5) LMER - lmer.R
6) clustering - clustering.R file (this file is for clustering questions based only on difficulty level)
7) clusteringanswertime.R file (this file is for clustering questions based on difficulty level and avg answer time)

Packages to be installed before running:
----------------------------------------
data.table, e1071, HMM, lme4, cluster, fpc


Running Instructions:
---------------------
Run preprocessing.R file first to get pre-processed and sampled data.
Then run individual classsifer files on the sampled data to get their predictions for each classifier in a csv file
Run clustering files individually.

