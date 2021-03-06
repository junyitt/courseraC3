#CodeBook

##Variables

The variables in tidy.txt are:

1. subject
2. activitylabel
3. tbodyacc.mean...x
4. tbodyacc.mean...y
5. tbodyacc.mean...z
6. tbodyacc.std...x
7. tbodyacc.std...y
8. tbodyacc.std...z
9. tgravityacc.mean...x
10. tgravityacc.mean...y
11. tgravityacc.mean...z
12. tgravityacc.std...x
13. tgravityacc.std...y
14. tgravityacc.std...z
15. tbodyaccjerk.mean...x
16. tbodyaccjerk.mean...y
17. tbodyaccjerk.mean...z
18. tbodyaccjerk.std...x
19. tbodyaccjerk.std...y
20. tbodyaccjerk.std...z
21. tbodygyro.mean...x
22. tbodygyro.mean...y
23. tbodygyro.mean...z
24. tbodygyro.std...x
25. tbodygyro.std...y
26. tbodygyro.std...z
27. tbodygyrojerk.mean...x
28. tbodygyrojerk.mean...y
29. tbodygyrojerk.mean...z
30. tbodygyrojerk.std...x
31. tbodygyrojerk.std...y
32. tbodygyrojerk.std...z
33. tbodyaccmag.mean..
34. tbodyaccmag.std..
35. tgravityaccmag.mean..
36. tgravityaccmag.std..
37. tbodyaccjerkmag.mean..
38. tbodyaccjerkmag.std..
39. tbodygyromag.mean..
40. tbodygyromag.std..
41. tbodygyrojerkmag.mean..
42. tbodygyrojerkmag.std..
43. fbodyacc.mean...x
44. fbodyacc.mean...y
45. fbodyacc.mean...z
46. fbodyacc.std...x
47. fbodyacc.std...y
48. fbodyacc.std...z
49. fbodyacc.meanfreq...x
50. fbodyacc.meanfreq...y
51. fbodyacc.meanfreq...z
52. fbodyaccjerk.mean...x
53. fbodyaccjerk.mean...y
54. fbodyaccjerk.mean...z
55. fbodyaccjerk.std...x
56. fbodyaccjerk.std...y
57. fbodyaccjerk.std...z
58. fbodyaccjerk.meanfreq...x
59. fbodyaccjerk.meanfreq...y
60. fbodyaccjerk.meanfreq...z
61. fbodygyro.mean...x
62. fbodygyro.mean...y
63. fbodygyro.mean...z
64. fbodygyro.std...x
65. fbodygyro.std...y
66. fbodygyro.std...z
67. fbodygyro.meanfreq...x
68. fbodygyro.meanfreq...y
69. fbodygyro.meanfreq...z
70. fbodyaccmag.mean..
71. fbodyaccmag.std..
72. fbodyaccmag.meanfreq..
73. fbodybodyaccjerkmag.mean..
74. fbodybodyaccjerkmag.std..
75. fbodybodyaccjerkmag.meanfreq..
76. fbodybodygyromag.mean..
77. fbodybodygyromag.std..
78. fbodybodygyromag.meanfreq..
79. fbodybodygyrojerkmag.mean..
80. fbodybodygyrojerkmag.std..
81. fbodybodygyrojerkmag.meanfreq..
82. angle.tbodyaccmean.gravity.
83. angle.tbodyaccjerkmean..gravitymean.
84. angle.tbodygyromean.gravitymean.
85. angle.tbodygyrojerkmean.gravitymean.
86. angle.x.gravitymean.
87. angle.y.gravitymean.
88. angle.z.gravitymean.

##Merge
First, the training and test sets were merged to create one data set. 

##Select
Then, columns 3 to 88 of the tidy data set (tidy.txt) were selected, where they represent the mean and standard deviation for each measurement.

##Activity labels
Then, the activity ID were replaced by descriptive activity labels.

##Variables names
The variable names were made unique and lower case.

##Tidy data set (average measurement)
Finally, the data set were grouped by subject and activity, where the mean of the 3rd variable to the 88th variable were calculated for each activity and each subject.


The details of variables of each feature vector is available in 'features_info.txt' in the data file downloaded via this link: <https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip>

