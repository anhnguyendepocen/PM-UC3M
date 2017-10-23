<!-- README.md is generated from README.Rmd. Please edit that file -->
Notes for *Predictive Modeling*
===============================

[![Travis-CI Build Status](https://travis-ci.org/egarpor/SSS2-UC3M.svg?branch=master)](https://travis-ci.org/egarpor/PM-UC3M) [![License](https://img.shields.io/badge/license-CC_BY--NC--SA_4.0-blue.svg)](https://creativecommons.org/licenses/by-nc-sa/4.0/)

Overview
--------

These are the notes for *Predictive Modeling* for the course 2017/2018. The subject is part of the [MSc in Big Data Analytics](https://www.uc3m.es/ss/Satellite/Postgrado/en/Detalle/Estudio_C/1371210340413/1371219633369/Master_in_Big_Data_Analytics) from [Carlos III University of Madrid](http://www.uc3m.es/).

The notes are available at <https://bookdown.org/egarpor/PM-UC3M>.

Here is a broad view of the **syllabus**:

1.  [Introduction](https://bookdown.org/egarpor/PM-UC3M/intro.html)
2.  [Linear models I](https://bookdown.org/egarpor/PM-UC3M/lm-i.html)
3.  [Linear models II](https://bookdown.org/egarpor/PM-UC3M/lm-ii.html)
4.  [Generalized linear models](https://bookdown.org/egarpor/PM-UC3M/glm.html)
5.  [Generalized additive models](https://bookdown.org/egarpor/PM-UC3M/gam.html)
6.  [Nonparametric regression](https://bookdown.org/egarpor/PM-UC3M/npreg.html)
7.  [Regression trees](https://bookdown.org/egarpor/PM-UC3M/trees.html)

**Office hours**: Thursdays from 19:15 to 20:15, at the lab in which the lesson took place.

Evaluation
----------

Evaluation is done by means of **four group reports**, to be done in groups of 3 (preferable) or 2 students, plus a presentation of one randomly-selected report. The final grade is:

    0.70 * (reports_grade) + 0.30 * (individual_presentation_grade).

The `reports_grade` is the weighted grade of the four reports. The reports cover the course topics:

1.  Linear models I and II (weight: 40%; **deadline: 2017/12/14**).
2.  Generalized linear models (weight: 20%; **deadline: 2017/12/21**).
3.  Generalized additive models (weight: 20%; **deadline: 2018/01/04**).
4.  Nonparametric regression (weight: 20%; **deadline: 2017/01/25**).

### Group reports

#### Groups

Team up in groups of 3 (preferable) or 2 members. Other group sizes are not allowed. It is up to you to form the groups based on your grade expectations, affinity, complementary skills, etc. All the students in a group will be graded evenly for the `reports_grade`.

Communicate the group compositions by filling this [Google Sheet](https://docs.google.com/a/uc3m.es/spreadsheets/d/10zWtuhpAEtfZs7tuL9wEPdGsLVVYT-MKDLudvuhEFCA/edit?usp=sharing) (you need to log in with your UC3M account). An example: if students A, B, and C form the group number `4` in the third report, add a `4` to the the rows of A, B, and C in the `Report III` column. Keep the same numbers if the group does not change. It is possible to switch group members from project to project, but be advised this might have undesirable consequences (such as someone having to participate in two different randomly-chosen presentations).

#### Aim of the reports

You will analyze a real dataset of your choice using the statistical methodology that we have seen in the lessons and labs. The purpose is to demonstrate that you know how to apply and interpret some of the studied statistical techniques in a real-case scenario that is appealing for you.

#### Report structure

*Mandatory* format guidelines:

-   Structure: title, authors, abstract, first section, second section and so on. Like [this](http://cje.oxfordjournals.org/content/38/2/257.full.pdf+html). Do not use a cover.
-   Font size: 11 points.
-   Spacing: single space, single column.
-   Length: **10 pages at most**. This is including tables, graphics, code, equations, appendices, etc. You are *not* required to make use of all the space. Want extra space? You can buy it at `-1.00` points per extra page.
-   Code: do not include *all* the code in the report. Instead of, just add the parts you want to highlight and comment their outputs or general ideas.
-   Style: be concise, specific, and insightful. Make a clever use of the space.

Use the following *mandatory* structure when writing your report:

1.  **Abstract**. Provide a concise summary of the project. It must not exceed 250 words.
2.  **Introduction**. State what is the problem to be studied. Provide some context, the question(s) that you want to address, a motivation of its importance, references, etc. Remember how we introduced the case studies covered in the course as a template (but you will need to elaborate more).
3.  **Methods**.
4.  **Statistical analysis**. Make use of some of the aforementioned statistical techniques, the ones that are more convenient to your particular case study. You can choose between covering several at a more superficial level, or one or two in more depth. Justify their adequacy and obtain analyses, explaining how you did it, in the form of plots and summaries. Provide a critical discussion about the outputs and give insights about them.
5.  **Conclusions**. Summary of what was addressed in the project and of the most important conclusions. Takeaway messages. The conclusions are not required to be spectacular, but *fair and honest* in terms of what you discovered.
6.  **References**. Refer to the sources of information that you have employed (for the data, for information on the data, for the statistical analyses, etc).

#### Hand in of reports

Send **one** email per group to <edgarcia@est-econ.uc3m.es> with the subject "\[PM - Report X - Group Y\]" (replace X and Y as convenience) and write the group members in the body of the email. Attach all relevant scripts and document the code appropriately.

Deadline for each report: in general, two weeks after we cover the report's topic, **at 23:59**. See the list of deadlines above. Recall that the last two deadlines, in the 25th and 28th of January, quite close to the presentation day, the 30th of January. Be prepared for that by not exhausting the deadlines!

Need extra time? Buy it at expenses of a penalization in your grade: `-0.04` points per extra hour. (Not applicable beyond the final deadline on the 28th of January -- the exposition takes place on the 30th of January!)

``` r
# Requires lubridate
# install.packages("lubridate")
library(lubridate)

# Continuous discounting, each extra hour penalizes with -0.04 points
penalization <- function(date, deadline) {
  
  stopifnot(is.POSIXct(date) & is.POSIXct(deadline))
  as.numeric(date - deadline, "hours") * 0.04
  
}

# Example of maximum grade of an exercise handed in at 03:31:59
10 - penalization(date = ymd_hms("2017-12-15 03:31:59"), 
                  deadline = ymd_hms("2017-12-14 23:59:59"))
#> [1] 9.858667
```

Graphically: ![](README/README-unnamed-chunk-3-1.png)

#### Grading

All students in a group will be graded evenly. Take this into account when forming the group. The grading is on a scale of 0-10 (plus 2 bonus points) and will be performed according to the following breakdown:

-   **Originality** of the problem studied and data acquisition process (up to 2 points).
-   **Statistical analyses presented** and their depth (up to 3 points). At least two different techniques should be employed (simple and multiple linear regression count as different, but the use of other techniques as well is mostly encouraged). Graded depending on their adequacy to the problem studied and the evidence you demonstrate about your knowledge of them.
-   Accuracy of the **interpretation** of the analyses (up to 2 points). Graded depending on the detail and rigor of the insights you elaborate from the statistical analyses.
-   **Reproducibility** of the study (1.5 point). Awarded if the code for reproducing the study, as well as the data, is provided in a ready-to-use way (e.g. the outputs from `R Commander`'s report mode along with the data).
-   **Presentation** of the report (1.5 point). This involves the correct usage of English, the readability, the conciseness and the overall presentation quality.
-   **Excellence** (2 bonus points). Awarded for creativity of the analysis, use of advanced statistical tools, use of points briefly covered in lessons/labs, advanced insights into the methods, completeness of the report, use of advanced presentation tools, etc. Only awarded if the sum of regular points is above 7.5.

The ratio "quality project"/"group size" might be taken into account in extreme cases (e.g. poor report written by 5 people, extremely good report written by 3 people).

### Report presentation

Save the date: **30th of January**, from 09:00 to 21:00 in room 0.B.08. Prior to the 30th, a randomly and a list of participation. Restrictions, if any, must be communicated in advance. There will be a list for

-   Each group must present one report, with an strict **15-minutes limit** for the whole presentation. Tailor your presentation to that duration and break evenly the time among the group members, such that all students will talk for at least 5 minutes.
-   All group members must be present in the presentation. Failure to be at the presentation results in `individual_presentation_grade = 0` for the missing members.
-   The presentation must follow tightly the report. The addition of new content is not permitted. Take this into account when preparing the reports.
-   Highlight your main contributions in the work and show your understanding of the methods.
-   The presentation grade is individual. Questions will be asked during and after the presentation to each student. Together with the presentation, the accurateness, conciseness and details of the answers will determine the grade.

### Academic fraud

Evidences of academic fraud will have serious consequences, such as a zero grade for the whole group and the reporting of the fraud detection to the pertinent academic authorities. Academic fraud includes (but is not limited to) plagiarism, use of sources without proper credit, project outsourcing, and the use of external tutoring not mentioned explicitly.

Contributions
-------------

Contributions, reporting of typos, and feedback on the notes are very welcome. Either send an email to <edgarcia@est-econ.uc3m.es> or (preferably) fork the repository, make your changes and open a pull request. Give me a reason for writing your name in a list of contributors!

License
-------

All material in this repository is licensed under [CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/).
