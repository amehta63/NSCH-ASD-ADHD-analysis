clear all
cls

use "data/nsch_2022e_topical.dta"

quietly {
label var a1_active  "Adult 1 - Active Duty"
label var a1_age  "Adult 1 - Age in Years"
label var a1_born  "Adult 1 - Where Born"
label var a1_deplstat  "Adult 1 - Deployment Status"
label var a1_employed  "Adult 1 - Current Employment Status"
label var a1_grade  "Adult 1 - Highest Completed Year of School"
label var a1_grade_if  "Imputation Flag for A1_GRADE"
label var a1_liveusa  "Adult 1 - Come to Live in the United States (Year)"
label var a1_marital  "Adult 1 - Marital Status"
label var a1_menthealth  "Adult 1 - Mental or Emotional Health"
label var a1_physhealth  "Adult 1 - Physical Health"
label var a1_relation  "Adult 1 - How Related to Child"
label var a1_sex  "Adult 1 - Sex"
label var a2_active  "Adult 2 - Active Duty"
label var a2_age  "Adult 2 - Age in Years"
label var a2_born  "Adult 2 - Where Born"
label var a2_deplstat  "Adult 2 - Deployment Status"
label var a2_employed  "Adult 2 - Current Employment Status"
label var a2_grade  "Adult 2 - Highest Completed Year of School"
label var a2_liveusa  "Adult 2 - Come to Live in the United States (Year)"
label var a2_marital  "Adult 2 - Marital Status"
label var a2_menthealth  "Adult 2 - Mental or Emotional Health"
label var a2_physhealth  "Adult 2 - Physical Health"
label var a2_relation  "Adult 2 - How Related to Child"
label var a2_sex  "Adult 2 - Sex"
label var ace1  "Hard to Cover Basics Like Food or Housing"
label var ace10  "Child Experienced - Treated Unfairly Because of Race"
label var ace11  "Child Experienced - Treated Unfairly Because of Health Condition"
label var ace12  "Child Experienced - Treated Unfairly Because of their Sexual Orientation or Gender Identity"
label var ace3  "Child Experienced - Parent or Guardian Divorced"
label var ace4  "Child Experienced - Parent or Guardian Died"
label var ace5  "Child Experienced - Parent or Guardian Time in Jail"
label var ace6  "Child Experienced - Adults Slap, Hit, Kick, Punch Others"
label var ace7  "Child Experienced - Victim of Violence"
label var ace8  "Child Experienced - Lived with Mentally Ill"
label var ace9  "Child Experienced - Lived with Person with Alcohol/Drug Problem"
label var addtreat  "ADD/ADHD - Behavioral Treatment"
label var agepos4  "Birth Order of Selected Children in Household"
label var allergies  "Allergies"
label var allergies_curr  "Allergies Currently"
label var allergies_desc  "Allergies Severity Description"
label var althealth  "Alternative Health Care"
label var appointment  "Needed Health Care Not Received Due to - Getting Appointment"
label var arrangehc  "Hours Spent Arranging Health Medical Care"
label var askquestion  "Ask Questions: Who, What, When, Where"
label var askquestion2  "Ask Questions:  Why, How"
label var athomehc  "Hours Spent Providing Home Health Care"
label var autismmed  "Autism ASD - Medication Currently"
label var autismtreat  "Autism ASD - Behavioral Treatment"
label var autoimmune  "Autoimmune Disease"
label var autoimmune_desc  "Autoimmune Disease Description"
label var available  "Needed Health Care Not Received Due to - Not Available"
label var avoidchg  "Past 12 Months - Avoided Changing Jobs to Maintain Health Insurance"
label var bedtime  "How Often - Go to Bed at Same Time"
label var bestforchild  "How Often - Work to Decide Together Treatment"
label var birthwt  "Birth Weight Status"
label var birthwt_l  "Birth Weight is Low (<2500g)"
label var birthwt_oz_s  "Standardized Birth Weight, Ounces"
label var birthwt_vl  "Birth Weight is Very Low (<1500g)"
label var birth_mo  "Birth Month"
label var birth_yr  "Birth Year"
label var birth_yr_f  "BIRTH_YR Data Quality Flag"
label var blindness  "Blindness"
label var blood  "Blood Disorder"
label var blood_desc  "Blood Disorder Severity Description"
label var blood_other  "Blood Disorder Other (includes Hemophilia)"
label var blood_screen  "Blood Disorder Newborn Screening"
label var bmiclass  "Body Mass Index, Percentile"
label var bodyimage  "Child's concern over body image "
label var bornusa  "Born in the United States"
label var bounceaball  "Bounce Ball"
label var breastfedend_day_s  "Stopped Breastfeeding - Days (Standardized)"
label var breastfedend_mo_s  "Stopped Breastfeeding - Months (Standardized)"
label var breastfedend_wk_s  "Stopped Breastfeeding - Weeks (Standardized)"
label var breathing  "Difficulty Breathing Past 12 Months"
label var bullied_r  "Bullied, Picked On, or Excluded by Others"
label var bully  "Bullies Others, Picks on Them, or Excludes Them"
label var c4q04  "Frustrated In Efforts to Get Service"
label var calmdown_r  "How Often - Calms Down When Excited"
label var cavities  "Difficulty Cavities Past 12 Months"
label var cbsafp_yn  "Core Based Statistical Area Status"
label var cerpals_desc  "Cerebral Palsy Severity Description"
label var changeage  "Doctor Worked with Child to Understand Health Care Changes"
label var clearexp  "How Often - Explain Things Good Idea"
label var concussion  "Concussion/Brain Injury"
label var confirminjury  "Concussion/Brain Injury - Confirmed Injury"
label var coordination  "Difficulty Coordination Past 12 Months"
label var countto_r  "Can Count How High"
label var covidarrange  "COVID - Closed School or Daycare"
label var covidcheckups  "COVID - Missed Preventative Check-Ups"
label var currcov  "Health Insurance Coverage - Currently Covered (Use CURRINS)"
label var currins  "Health Insurance Coverage - Currently Covered"
label var cuthours  "Past 12 Months -Cut Hours because of Health Conditions"
label var cystfib  "Cystic Fibrosis"
label var cystfib_screen  "Cystic Fibrosis Newborn Screening"
label var decisions  "Health Care Decisions Needed"
label var dentalserv1  "Dental Service - Check-up"
label var dentalserv2  "Dental Service - Cleaning"
label var dentalserv3  "Dental Service - Instructions on Toothbrushing"
label var dentalserv4  "Dental Service - X-Rays"
label var dentalserv5  "Dental Service - Fluoride Treatment"
label var dentalserv6  "Dental Service - Sealant"
label var dentalserv7  "Dental Service - Don't Know"
label var dentistvisit  "Preventive Dental Visit - How Many Visits"
label var diabetes  "Diabetes"
label var diabetes_curr  "Diabetes Currently"
label var diabetes_desc  "Diabetes Severity Description"
label var directions  "Follow Verbal Directions"
label var directions2  "Follow Two Step Directions"
label var discussopt  "How Often - Provider Discussed Range of Options"
label var distracted  "How Often - Easily Distracted"
label var docprivate  "Child Spoke with Doctor Privately"
label var docroom  "Preventive Visit - How Long with Doctor"
label var downsyn  "Down Syndrome"
label var drawacircle  "Draw a Circle"
label var drawaface  "Draw a Face"
label var drawaperson  "Draw a Person"
label var dressing  "Difficulty Dressing or Bathing"
label var ebtcards  "School Meal Debit "
label var emosupadv  "Emotional Support - Health Condition Support Group"
label var emosupfam  "Emotional Support - Other Family or Friend"
label var emosuphcp  "Emotional Support - Health Care Provider"
label var emosupmhp  "Emotional Support - Counselor"
label var emosupoth  "Emotional Support - Other"
label var emosuppeer  "Emotional Support - Peer Support Group"
label var emosupspo  "Emotional Support - Spouse or Domestic Partner"
label var emosupwor  "Emotional Support - Place of Worship"
label var engageconcern  "Concerned over Child's Engagement in Eating Behaviors"
label var engage_binge  "Eating Engagement - Binging"
label var engage_exercise  "Eating Engagement - Over-Exercise"
label var engage_fast  "Eating Engagement - Fasting"
label var engage_interest  "Eating Engagement - No Interest"
label var engage_noeat  "Eating Engagement - Not Eating"
label var engage_picky  "Eating Engagement - Picky"
label var engage_pills  "Eating Engagement - Pills"
label var engage_purg  "Eating Engagement - Purging"
label var errandalone  "Difficulty Doing Errands Alone"
label var evalfasd  "Evaluation for a Fetal Alcohol Spectrum Disorder Recommended"
label var everhomeless  "Homeless or Lived in Shelter "
label var eyecare1  "Eye Doctor Care - Examination"
label var eyecare2  "Eye Doctor Care - Corrective Lenses"
label var eyecare3  "Eye Doctor Care - Vision Disorder"
label var eyecare4  "Eye Doctor Care - Some other care"
label var eyedoctor  "Visition Tested by Eye Doctor"
label var famcount  "Number of People That Are Family Members"
label var family_r  "Family Structure"
label var fasd  "Fetal Alcohol Spectrum Disorder (FASD)"
label var fipsst  "State FIPS Code"
label var focuson  "Focus on a Task"
label var foodsit  "Food Situation In Household - Past 12 Months"
label var formtype  "Form Type"
label var fpl_i1  "Family Poverty Ratio, First Implicate"
label var fpl_i2  "Family Poverty Ratio, Second Implicate"
label var fpl_i3  "Family Poverty Ratio, Third Implicate"
label var fpl_i4  "Family Poverty Ratio, Fourth Implicate"
label var fpl_i5  "Family Poverty Ratio, Fifth Implicate"
label var fpl_i6  "Family Poverty Ratio, Sixth Implicate"
label var fpl_if  "Imputation Flag for FPL"
label var frstformula_day_s  "First Fed Formula - Days (Standardized)"
label var frstformula_mo_s  "First Fed Formula - Months (Standardized)"
label var frstformula_wk_s  "First Fed Formula - Weeks (Standardized)"
label var frstsolids_day_s  "First Fed Solids - Days (Standardized)"
label var frstsolids_mo_s  "First Fed Solids - Days (Standardized)"
label var frstsolids_wk_s  "First Fed Solids - Days (Standardized)"
label var fruit  "How Many Fruits"
label var fwc  "Selected Child Weight"
label var gainskills  "Doctor Worked with Child to Gain Skills to Manage Health"
label var genetic  "Genetic Condition"
label var genetic_desc  "Genetic Condition Severity Description"
label var genetic_screen  "Genetic Condition Newborn Screening"
label var goforhelp  "Know Where to Go For Help"
label var grades  "Grades Received in School "
label var groupofobjects  "Group of Objects"
label var gumbleed  "Difficulty Bleeding Gums Past 12 Months"
label var hands  "Difficulty Hands Past 12 Months"
label var hardwork  "How Often - Work even when hard "
label var hcability  "Health Affected Ability - How Often"
label var hccovoth  "Health Insurance - Other"
label var hcextent  "Health Affected Ability - Extent"
label var headache  "Headaches"
label var headache_curr  "Headaches Currently"
label var headache_desc  "Headaches Severity Description"
label var healthknow  "How Child Will Be Insured as an Adult"
label var heart  "Heart Condition"
label var heart_born  "Heart Born"
label var heart_curr  "Heart Condition Currently"
label var heart_desc  "Heart Condition Severity Description"
label var hemophilia  "Blood Disorder Hemophilia"
label var hhcount  "Number of People Living at Address"
label var hhcount_if  "Imputation Flag for HHCOUNT"
label var hhid  "Unique Household ID"
label var hhlanguage  "Primary Household Language"
label var higrade  "Highest Level of Education among Reported Adults"
label var higrade_tvis  "Highest Level of Education among Reported Adults, Detail"
label var homeevic  "Worried about Being Evicted "
label var hopeful  "Facing Problems - How Often Stay Hopeful"
label var hospitaler  "Hospital Emergency Room Visits"
label var hospitalstay  "Admitted to Hospital"
label var hoursleep  "Past Week - How Many Hours of Sleep Average"
label var hoursleep05  "Past Week - How Many Hours of Sleep Average"
label var house_gen  "Parental Nativity"
label var howmuch  "How Much Medical Health Care - Past 12 Months"
label var hurtsad  "How Often - Show Concern"
cap label var inq_edu  "Black-White educational inequity"
cap label var inq_employ  "Black-White employment inequity"
cap label var inq_home  "Black-White homeownership inequity"
cap label var inq_income  "Black-White income inequity"
cap label var inq_resseg  "Black-White Residential Segregation"
label var insgap  "Health Insurance Coverage - Past 12 Months"
label var instype  "Insurance Type (Revised)"
label var issuecost  "Needed Health Care Not Received Due to - Cost"
label var k10q11  "Neighborhood - Sidewalks or Walking Paths"
label var k10q12  "Neighborhood - Park or Playground"
label var k10q13  "Neighborhood - Recreation Center"
label var k10q14  "Neighborhood - Library or Bookmobile"
label var k10q20  "Neighborhood - Litter or Garbage"
label var k10q22  "Neighborhood - Poorly Kept or Rundown Housing"
label var k10q23  "Neighborhood - Vandalism"
label var k10q30  "People In Neighborhood Help Each Other Out"
label var k10q31  "Watch Out for Other's Children"
label var k10q40_r  "Child is Safe In Neighborhood"
label var k10q41_r  "Child Is Safe at School"
label var k11q03r  "Health Insurance - Indian Health Service"
label var k11q43r  "How Many Times Moved to New Address"
label var k11q60  "Cash Assistance from Government - Past 12 Months"
label var k11q61  "Food Stamps - Past 12 Months"
label var k11q62  "Free or Reduced Cost Meals - Past 12 Months"
label var k12q01_a  "Reason Not Covered - Change in Employer/Employment"
label var k12q01_b  "Reason Not Covered - Cancellation Overdue Premiums"
label var k12q01_c  "Reason Not Covered - Unaffordable"
label var k12q01_d  "Reason Not Covered - Inadequate Benefits"
label var k12q01_e  "Reason Not Covered - Inadequate Providers"
label var k12q01_f  "Reason Not Covered - Application/Renewal Problems"
label var k12q01_g  "Reason Not Covered - Other"
label var k12q03  "Health Insurance - Current/Former Employer or Union"
label var k12q04  "Health Insurance - Insurance Company"
label var k12q12  "Health Insurance - Government Assistance Plan"
label var k2q01  "General Health"
label var k2q01_d  "Teeth Description"
label var k2q05  "Born 3 or More Weeks Before Due Date"
label var k2q30a  "Learning Disability"
label var k2q30b  "Learning Disability Currently"
label var k2q30c  "Learning Disability Severity Description"
label var k2q31a  "ADD/ADHD"
label var k2q31b  "ADD/ADHD Currently"
label var k2q31c  "ADD/ADHD Severity Description"
label var k2q31d  "ADD/ADHD - Medication Currently"
label var k2q32a  "Depression"
label var k2q32b  "Depression Currently"
label var k2q32c  "Depression Severity Description"
label var k2q33a  "Anxiety"
label var k2q33b  "Anxiety Currently"
label var k2q33c  "Anxiety Severity Description"
label var k2q34a  "Behavior Problems"
label var k2q34b  "Behavior Problems Currently"
label var k2q34c  "Behavior Problems Severity Description"
label var k2q35a  "Autism ASD"
label var k2q35a_1_years  "Autism ASD - First Told Age in Years"
label var k2q35b  "Autism ASD Currently"
label var k2q35c  "Autism ASD Severity Description"
label var k2q35d  "Autism ASD - First Told Doctor Type"
label var k2q36a  "Developmental Delay"
label var k2q36b  "Developmental Delay Currently"
label var k2q36c  "Developmental Delay Severity Description"
label var k2q37a  "Speech Disorder"
label var k2q37b  "Speech Disorder Currently"
label var k2q37c  "Speech Disorder Severity Description"
label var k2q38a  "Tourette Syndrome"
label var k2q38b  "Tourette Syndrome Currently"
label var k2q38c  "Tourette Syndrome Severity Description"
label var k2q40a  "Asthma"
label var k2q40b  "Asthma Currently"
label var k2q40c  "Asthma Severity Description"
label var k2q42a  "Epilepsy"
label var k2q42b  "Epilepsy Currently"
label var k2q42c  "Epilepsy Severity Description"
label var k2q43b  "Deafness"
label var k2q60a  "Intellectual Disability"
label var k2q60b  "Intellectual Disability Currently"
label var k2q60c  "Intellectual Disability Severity Description"
label var k2q61a  "Cerebral Palsy"
label var k3q04_r  "Health Insurance Coverage - Past 12 Months (Use INSGAP)"
label var k3q20  "Health Insurance - Benefits Cover Services"
label var k3q21b  "How Often Costs Reasonable"
label var k3q22  "Health Insurance - Allow to See Provider"
label var k3q25  "Problems Paying for Medical or Health Care"
label var k4q01  "Place Usually Goes Sick"
label var k4q02_r  "Place Usually Goes Sick - Where"
label var k4q04_r  "Personal Doctor or Nurse - One or More"
label var k4q20r  "Preventive Visit - How Many Times"
label var k4q22_r  "Mental Health Professional Treatment"
label var k4q23  "Emotions Concentration Behavior Medication"
label var k4q24_r  "Specialist Visit"
label var k4q26  "Specialist Visit - Problem"
label var k4q27  "Needed Health Care Not Received"
label var k4q28x01  "Needed Health Care Not Received - Medical Care"
label var k4q28x02  "Needed Health Care Not Received - Dental Care"
label var k4q28x03  "Needed Health Care Not Received - Vision Care"
label var k4q28x04  "Needed Health Care Not Received - Mental Health Services"
label var k4q28x05  "Needed Health Care Not Received - Other"
label var k4q28x_ear  "Needed Health Care Not Received - Hearing Care"
label var k4q30_r_1  "Dental Provider Visit - Dentist"
label var k4q30_r_2  "Dental Provider Visit - Other"
label var k4q30_r_3  "Dental Provider Visit - No"
label var k4q36  "Received Special Services"
label var k4q37  "Received Special Services - Age in Years"
label var k4q38  "Received Special Services - Currently"
label var k5q10  "Need a Referral"
label var k5q11  "Need a Referral - Problem"
label var k5q20_r  "Arrange Or Coordinate Care Among Doctors"
label var k5q21  "Arrange Or Coordinate Care Extra Help"
label var k5q22  "Arrange Or Coordinate As Much Help As Wanted"
label var k5q30  "Communication Satisfaction Among Doctors"
label var k5q31_r  "Provider Communication with School, Child Care, Special Education Program"
label var k5q32  "Communication Satisfaction with School, Child Care, Special Education Program"
label var k5q40  "How Often - Spend Enough Time"
label var k5q41  "How Often - Listen Carefully"
label var k5q42  "How Often - Show Sensitivity"
label var k5q43  "How Often - Provide Specific Information"
label var k5q44  "How Often - Feel Like a Partner"
label var k6q10  "Asked about Learning, Development, Behavior Concerns"
label var k6q12  "Questionnaire - Development Concerns"
label var k6q13a  "Questionnaire Covers Talks or Speech Sounds Concerns"
label var k6q13b  "Questionnaire Covers Interaction Concerns"
label var k6q14a  "Questionnaire Covers Words and Phrases Concerns"
label var k6q14b  "Questionnaire Covers Behaves and Gets Along Concerns"
label var k6q15  "Special Education Plan"
label var k6q20  "Receive Care From Others at Least 10 Hours Per Week"
label var k6q27  "Job Change Because Problems with Child Care"
label var k6q40  "Ever Breastfed"
label var k6q41r_still  "Stopped Breastfeeding - Still Breastfeeding"
label var k6q42r_never  "First Fed Formula - Never"
label var k6q43r_never  "First Fed Other - Never"
label var k6q60_r  "How Many Days Read to Child"
label var k6q61_r  "How Many Days Tell Stories or Sing to Child"
label var k6q70_r  "Affectionate"
label var k6q71_r  "Show Interest and Curiosity"
label var k6q72_r  "Smiles Laughs"
label var k6q73_r  "Bounces Back"
label var k7q02r_r  "Days Child Missed School - Illness or Injury"
label var k7q04r_r  "Times School Contacted Household About Problems"
label var k7q30  "Sports Team or Sports Lessons - Past 12 Months"
label var k7q31  "Clubs or Organizations - Past 12 Months"
label var k7q32  "Organized Activities or Lessons - Past 12 Months"
label var k7q33  "How Often Attend Events or Activities"
label var k7q37  "Community Service or Volunteer Work - Past 12 Months"
label var k7q38  "Paid Work or Regular Job - Past 12 Months"
label var k7q70_r  "Argues Too Much"
label var k7q82_r  "Cares About Doing Well in School"
label var k7q83_r  "Does All Required Homework"
label var k7q84_r  "Works to Finish Tasks Started"
label var k7q85_r  "Stays Calm and In Control When Challenged"
label var k8q11  "How Many Days - Family Eat Meal Together"
label var k8q21  "Share Ideas or Talk About Things That Matter"
label var k8q30  "How Well Handling Demands of Raising Children"
label var k8q31  "How Often Have You Felt - Child Hard to Care For"
label var k8q32  "How Often Have You Felt - Child Really Bothers You"
label var k8q34  "How Often Have You Felt - Angry with Child"
label var k8q35  "Someone to Turn To for Emotional Support"
label var k9q40  "Anyone in Household Use Cigarettes"
label var k9q41  "Anyone Smoke Inside of Home"
label var k9q96  "Other Adult Child Can Rely On For Advice"
label var keepinsadult  "How to Obtain/Keep Health Insurance as Child Becomes Adult"
label var liveusa_mo  "How Long Living in the United States - Months"
label var liveusa_yr  "How Long Living in the United States - Years"
label var makefriend  "Difficulty Making or Keeping Friends"
label var medhistory  "Receive Summary of Medical History"
label var memorycond  "Serious Difficulty Concentrating, Remembering, or Making Decisions"
label var menbevcov  "Health Insurance - Cover Mental Behavioral Needs"
label var metro_yn  "Metropolitan Statistical Area Status"
label var missmortgage  "Not Able to Pay Mortgage or Rent"
label var momage  "Age of Mother - Years"
label var mpc_yn  "Metropolitan Principal City Status"
label var nameemotions  "Recognize Emotion"
label var notelig  "Needed Health Care Not Received Due to - Not Eligible"
label var notopen  "Needed Health Care Not Received Due to - Office Not Open"
label var oneword  "Say One Word"
label var outdoorswkday  "Time Spent Playing Outdoors - Weekday"
label var outdoorswkend  "Time Spent Playing Outdoors - Weekend"
label var overweight  "Doctor Identified as Overweight"
label var physactiv  "Exercise, Play Sport, or Physical Activity for 60 Minutes"
label var physicalpain  "Difficulty Physical Pain Past 12 Months"
label var placeslived  "Number of placed the child has lived "
label var planneeds_r  "Plan Address Transition"
label var playwell  "How Often - Play Well with Others"
label var point  "Point to Things"
label var poschoice  "Doctor Worked with Child to Make Positive Choices"
label var raiseconc  "How Often - Easy to Raise Concerns or Disagree"
label var readonedigit  "Read One Digit"
label var receivecopy  "Have Access to Plan of Care"
label var recevalfasd  "Received an Evaluation for FASD"
label var recogabc  "Recognize Letters of Alphabet"
label var recogbegin  "Recognize Beginning Sound of a Word"
label var repeated  "Child Repeated Any Grades"
label var rhymeword_r  "Rhyme Words"
label var s4q01  "Doctor Visit"
label var s9q34  "WIC Benefits - Past 12 Months"
label var samesound  "Words with Same Start Sound"
label var screentime  "How Much Time Spent with TV, Cellphone, Computer"
label var sc_age_lt10  "Age of Selected Child - Less than 10 Months"
label var sc_age_lt4  "Age of Selected Child - Less than 4 Months"
label var sc_age_lt6  "Age of Selected Child - Less than 6 Months"
label var sc_age_lt9  "Age of Selected Child - Less than 9 Months"
label var sc_age_years  "Age of Selected Child - In Years"
label var sc_aian  "American Indian or Alaska Native Alone or in Combination with Other Race"
label var sc_asian  "Asian Alone or in Combination with Other Race"
label var sc_cshcn  "Special Health Care Needs Status of Selected Child"
label var sc_english  "SC  Speaks English"
label var sc_hispanic_r  "Hispanic Origin of Selected Child, Recode"
label var sc_hispanic_r_if  "Imputation Flag for SC_HISPANIC_R"
label var sc_k2q10  "SC Needs or Uses Medication Currently"
label var sc_k2q11  "SC Medication Used or Needed for Health Condition"
label var sc_k2q12  "SC Medication Currently for 12 Months"
label var sc_k2q13  "SC Needs or Uses More Medical Care than Others"
label var sc_k2q14  "SC Medical Care Used or Needed for Health Condition"
label var sc_k2q15  "SC Medical Care Currently for 12 Months"
label var sc_k2q16  "SC Limited Ability"
label var sc_k2q17  "SC Limited Ability from Health Condition"
label var sc_k2q18  "SC Limited Ability from Health Condition for 12 Months"
label var sc_k2q19  "SC Special Therapy"
label var sc_k2q20  "SC Special Therapy for Health Condition"
label var sc_k2q21  "SC Special Therapy for Health Condition for 12 Months"
label var sc_k2q22  "SC Needs Treatment for Emotion Develop Behave"
label var sc_k2q23  "SC Treatment for Chronic Emotion Develop Behave"
label var sc_nhpi  "Native Hawaiian or Other Pacific Islanders Alone or in Combination with Other Race"
label var sc_racer  "Race of Selected Child, Recode"
label var sc_race_r  "Race of Selected Child, Detailed"
label var sc_race_r_if  "Imputation Flag for SC_RACE_R"
label var sc_sex  "Sex of Selected Child"
label var sc_sex_if  "Imputation Flag for SC_SEX"
label var seekcare  "Concussion/Brain Injury - Seek Care"
label var sescurrsvc  "Special Education Plan - Currently"
label var sesplanmo  "Special Education Plan - Age in Months (use with SESPLANYR)"
label var sesplanyr  "Special Education Plan - Age in Years"
label var sharetoys  "Share Toys"
label var sicklecell  "Blood Disorder Sickle Cell"
label var simpleaddition  "Simple Addition"
label var sleeppos  "Position Most Often Lay Your Baby Down to Sleep"
label var spcservmo  "Received Special Services - Age in Months (use with K4Q37)"
label var ssi  "Receive SSI "
label var ssidisability  "Receive SSI for Disability "
label var startnewact  "How Often - Difficulty End/Starting Activities"
label var startschool  "Has Child Started School"
label var stomach  "Difficulty Stomach Past 12 Months"
label var stopwork  "Past 12 Months - Stopped Working because of Health Status"
label var stratum  "Sampling Stratum"
label var strengths  "Facing Problems - How Often Draw on Strengths"
label var sugardrink  "How Many Surgary Drinks"
label var swallowing  "Difficulty Swallowing Past 12 Months"
label var talkabout  "Facing Problems - How Often Talk Together"
label var tellstory  "Tell a Story"
label var temper_r  "How Often - Loses Temper"
label var tenure  "The Conditions under Which Land or Buildings Are Held or Occupied"
label var tenure_if  "Imputation Flag for TENURE"
label var thalassemia  "Blood Disorder Thalassemia"
label var threewords  "Use Three Words Together"
label var toothaches  "Difficulty Toothaches Past 12 Months"
label var totage_0_5  "Count of Children Ages 0 to 5 in Household"
label var totage_12_17  "Count of Children Ages 12 to 17 in Household"
label var totage_6_11  "Count of Children Ages 6 to 11 in Household"
label var totcshcn  "Count of Children with Special Health Care Needs in Household"
label var totfemale  "Count of Female Children in Household"
label var totkids_r  "Number of Children in Household"
label var totmale  "Count of Male Children in Household"
label var totnonshcn  "Count of Children without Special Health Care Needs in Household"
label var transportcc  "Needed Health Care Not Received Due to - Getting Transportation"
label var treatadult  "Talked About Child Seeing Doctors Who Treat Adults"
label var treatchild  "Doctors Treat Only Children"
label var treatneed  "Mental Health Professional Treatment - Problem"
label var tricare  "Health Insurance - TRICARE"
label var twowords  "Use Two Words Together"
label var understand  "Understand 'No'"
label var understand2  "Understand 'In', 'On', 'Under'"
label var usualgo  "Place Usually Goes for Preventive Care"
label var usualsick  "Place Usually Goes for Sick Same As Preventive"
label var vape  "Vape/E-Cigarettes inside of Home"
label var vegetables  "How Many Vegetables"
label var videophone  "COVID - Virtual Health Care Visits"
label var videophonecovid  "COVID - Virtual Health Care Visits Due to COVID"
label var visionexamrec  "Vision Test Recommended "
label var visionscreenother  "Vision Screening from Other Provider"
label var waitforturn  "How Often - Waits for Their Turn"
label var walkstairs  "Serious Difficulty Walking or Climbing Stairs"
label var wgtconc  "Concerned About Weight"
label var wktosolve  "Facing Problems - How Often Work Together"
label var writename  "How Often - Write First Name"
label var writeplan  "Doctor Worked with You and Child To Create Written Plan"
label var year  "Survey Year"
label var height  "HEIGHT"
label var weight  "WEIGHT"
label define a1_active_lab  1  "Never served in the military"
label define a1_active_lab  2  "Only on active duty for training in the Reserves or National Guard", add
label define a1_active_lab  3  "Now on active duty", add
label define a1_active_lab  4  "On active duty in the past, but not now", add
label define a1_active_lab  .m "No valid response", add
label define a1_active_lab  .n "Not in universe", add
label define a1_active_lab  .l "Logical skip", add
label define a1_active_lab  .d "Suppressed for confidentiality", add
cap label values a1_active a1_active_lab
label define a1_age_lab  .m "No valid response"
label define a1_age_lab  .n "Not in universe", add
label define a1_age_lab  .l "Logical skip", add
label define a1_age_lab  .d "Suppressed for confidentiality", add
cap label values a1_age a1_age_lab
label define a1_born_lab  1  "In the United States"
label define a1_born_lab  2  "Outside of the United States", add
label define a1_born_lab  .m "No valid response", add
label define a1_born_lab  .n "Not in universe", add
label define a1_born_lab  .l "Logical skip", add
label define a1_born_lab  .d "Suppressed for confidentiality", add
cap label values a1_born a1_born_lab
label define a1_deplstat_lab  1  "Yes"
label define a1_deplstat_lab  2  "No", add
label define a1_deplstat_lab  .m "No valid response", add
label define a1_deplstat_lab  .n "Not in universe", add
label define a1_deplstat_lab  .l "Logical skip", add
label define a1_deplstat_lab  .d "Suppressed for confidentiality", add
cap label values a1_deplstat a1_deplstat_lab
label define a1_employed_lab  1  "Employed full-time"
label define a1_employed_lab  2  "Employed part-time", add
label define a1_employed_lab  3  "Working WITHOUT pay", add
label define a1_employed_lab  4  "Not employed but looking for work", add
label define a1_employed_lab  5  "Not employed but not looking for work", add
label define a1_employed_lab  .m "No valid response", add
label define a1_employed_lab  .n "Not in universe", add
label define a1_employed_lab  .l "Logical skip", add
label define a1_employed_lab  .d "Suppressed for confidentiality", add
cap label values a1_employed a1_employed_lab
label define a1_grade_lab  1  "8th grade or less"
label define a1_grade_lab  2  "9th-12th grade; No diploma", add
label define a1_grade_lab  3  "High School Graduate or GED Completed", add
label define a1_grade_lab  4  "Completed a vocational, trade, or business school program", add
label define a1_grade_lab  5  "Some College Credit, but No Degree", add
label define a1_grade_lab  6  "Associate Degree (AA, AS)", add
label define a1_grade_lab  7  "Bachelor's Degree (BA, BS, AB)", add
label define a1_grade_lab  8  "Master's Degree (MA, MS, MSW, MBA)", add
label define a1_grade_lab  9  "Doctorate (PhD, EdD) or Professional Degree (MD, DDS, DVM, JD)", add
label define a1_grade_lab  .m "No valid response", add
label define a1_grade_lab  .n "Not in universe", add
label define a1_grade_lab  .l "Logical skip", add
label define a1_grade_lab  .d "Suppressed for confidentiality", add
cap label values a1_grade a1_grade_lab
label define a1_grade_if_lab  1  "Imputed"
label define a1_grade_if_lab  0  "Not imputed", add
label define a1_grade_if_lab  .m "No valid response", add
label define a1_grade_if_lab  .n "Not in universe", add
label define a1_grade_if_lab  .l "Logical skip", add
label define a1_grade_if_lab  .d "Suppressed for confidentiality", add
cap label values a1_grade_if a1_grade_if_lab
label define a1_liveusa_lab  .m "No valid response"
label define a1_liveusa_lab  .n "Not in universe", add
label define a1_liveusa_lab  .l "Logical skip", add
label define a1_liveusa_lab  .d "Suppressed for confidentiality", add
cap label values a1_liveusa a1_liveusa_lab
label define a1_marital_lab  1  "Married"
label define a1_marital_lab  2  "Not married, but living with a partner", add
label define a1_marital_lab  3  "Never Married", add
label define a1_marital_lab  4  "Divorced", add
label define a1_marital_lab  5  "Separated", add
label define a1_marital_lab  6  "Widowed", add
label define a1_marital_lab  .m "No valid response", add
label define a1_marital_lab  .n "Not in universe", add
label define a1_marital_lab  .l "Logical skip", add
label define a1_marital_lab  .d "Suppressed for confidentiality", add
cap label values a1_marital a1_marital_lab
label define a1_menthealth_lab  1  "Excellent"
label define a1_menthealth_lab  2  "Very Good", add
label define a1_menthealth_lab  3  "Good", add
label define a1_menthealth_lab  4  "Fair", add
label define a1_menthealth_lab  5  "Poor", add
label define a1_menthealth_lab  .m "No valid response", add
label define a1_menthealth_lab  .n "Not in universe", add
label define a1_menthealth_lab  .l "Logical skip", add
label define a1_menthealth_lab  .d "Suppressed for confidentiality", add
cap label values a1_menthealth a1_menthealth_lab
label define a1_physhealth_lab  1  "Excellent"
label define a1_physhealth_lab  2  "Very Good", add
label define a1_physhealth_lab  3  "Good", add
label define a1_physhealth_lab  4  "Fair", add
label define a1_physhealth_lab  5  "Poor", add
label define a1_physhealth_lab  .m "No valid response", add
label define a1_physhealth_lab  .n "Not in universe", add
label define a1_physhealth_lab  .l "Logical skip", add
label define a1_physhealth_lab  .d "Suppressed for confidentiality", add
cap label values a1_physhealth a1_physhealth_lab
label define a1_relation_lab  1  "Biological or Adoptive Parent"
label define a1_relation_lab  2  "Step-parent", add
label define a1_relation_lab  3  "Grandparent", add
label define a1_relation_lab  4  "Foster Parent", add
label define a1_relation_lab  6  "Other: Relative", add
label define a1_relation_lab  7  "Other: Non-Relative", add
label define a1_relation_lab  .m "No valid response", add
label define a1_relation_lab  .n "Not in universe", add
label define a1_relation_lab  .l "Logical skip", add
label define a1_relation_lab  .d "Suppressed for confidentiality", add
cap label values a1_relation a1_relation_lab
label define a1_sex_lab  1  "Male"
label define a1_sex_lab  2  "Female", add
label define a1_sex_lab  .m "No valid response", add
label define a1_sex_lab  .n "Not in universe", add
label define a1_sex_lab  .l "Logical skip", add
label define a1_sex_lab  .d "Suppressed for confidentiality", add
cap label values a1_sex a1_sex_lab
label define a2_active_lab  1  "Never served in the military"
label define a2_active_lab  2  "Only on active duty for training in the Reserves or National Guard", add
label define a2_active_lab  3  "Now on active duty", add
label define a2_active_lab  4  "On active duty in the past, but not now", add
label define a2_active_lab  .m "No valid response", add
label define a2_active_lab  .n "Not in universe", add
label define a2_active_lab  .l "Logical skip", add
label define a2_active_lab  .d "Suppressed for confidentiality", add
cap label values a2_active a2_active_lab
label define a2_age_lab  .m "No valid response"
label define a2_age_lab  .n "Not in universe", add
label define a2_age_lab  .l "Logical skip", add
label define a2_age_lab  .d "Suppressed for confidentiality", add
cap label values a2_age a2_age_lab
label define a2_born_lab  1  "In the United States"
label define a2_born_lab  2  "Outside of the United States", add
label define a2_born_lab  .m "No valid response", add
label define a2_born_lab  .n "Not in universe", add
label define a2_born_lab  .l "Logical skip", add
label define a2_born_lab  .d "Suppressed for confidentiality", add
cap label values a2_born a2_born_lab
label define a2_deplstat_lab  1  "Yes"
label define a2_deplstat_lab  2  "No", add
label define a2_deplstat_lab  .m "No valid response", add
label define a2_deplstat_lab  .n "Not in universe", add
label define a2_deplstat_lab  .l "Logical skip", add
label define a2_deplstat_lab  .d "Suppressed for confidentiality", add
cap label values a2_deplstat a2_deplstat_lab
label define a2_employed_lab  1  "Employed full-time"
label define a2_employed_lab  2  "Employed part-time", add
label define a2_employed_lab  3  "Working WITHOUT pay", add
label define a2_employed_lab  4  "Not employed but looking for work", add
label define a2_employed_lab  5  "Not employed but not looking for work", add
label define a2_employed_lab  .m "No valid response", add
label define a2_employed_lab  .n "Not in universe", add
label define a2_employed_lab  .l "Logical skip", add
label define a2_employed_lab  .d "Suppressed for confidentiality", add
cap label values a2_employed a2_employed_lab
label define a2_grade_lab  1  "8th grade or less"
label define a2_grade_lab  2  "9th-12th grade; No diploma", add
label define a2_grade_lab  3  "High School Graduate or GED Completed", add
label define a2_grade_lab  4  "Completed a vocational, trade, or business school program", add
label define a2_grade_lab  5  "Some College Credit, but No Degree", add
label define a2_grade_lab  6  "Associate Degree (AA, AS)", add
label define a2_grade_lab  7  "Bachelor's Degree (BA, BS, AB)", add
label define a2_grade_lab  8  "Master's Degree (MA, MS, MSW, MBA)", add
label define a2_grade_lab  9  "Doctorate (PhD, EdD) or Professional Degree (MD, DDS, DVM, JD)", add
label define a2_grade_lab  .m "No valid response", add
label define a2_grade_lab  .n "Not in universe", add
label define a2_grade_lab  .l "Logical skip", add
label define a2_grade_lab  .d "Suppressed for confidentiality", add
cap label values a2_grade a2_grade_lab
label define a2_liveusa_lab  .m "No valid response"
label define a2_liveusa_lab  .n "Not in universe", add
label define a2_liveusa_lab  .l "Logical skip", add
label define a2_liveusa_lab  .d "Suppressed for confidentiality", add
cap label values a2_liveusa a2_liveusa_lab
label define a2_marital_lab  1  "Married"
label define a2_marital_lab  2  "Not married, but living with a partner", add
label define a2_marital_lab  3  "Never Married", add
label define a2_marital_lab  4  "Divorced", add
label define a2_marital_lab  5  "Separated", add
label define a2_marital_lab  6  "Widowed", add
label define a2_marital_lab  .m "No valid response", add
label define a2_marital_lab  .n "Not in universe", add
label define a2_marital_lab  .l "Logical skip", add
label define a2_marital_lab  .d "Suppressed for confidentiality", add
cap label values a2_marital a2_marital_lab
label define a2_menthealth_lab  1  "Excellent"
label define a2_menthealth_lab  2  "Very Good", add
label define a2_menthealth_lab  3  "Good", add
label define a2_menthealth_lab  4  "Fair", add
label define a2_menthealth_lab  5  "Poor", add
label define a2_menthealth_lab  .m "No valid response", add
label define a2_menthealth_lab  .n "Not in universe", add
label define a2_menthealth_lab  .l "Logical skip", add
label define a2_menthealth_lab  .d "Suppressed for confidentiality", add
cap label values a2_menthealth a2_menthealth_lab
label define a2_physhealth_lab  1  "Excellent"
label define a2_physhealth_lab  2  "Very Good", add
label define a2_physhealth_lab  3  "Good", add
label define a2_physhealth_lab  4  "Fair", add
label define a2_physhealth_lab  5  "Poor", add
label define a2_physhealth_lab  .m "No valid response", add
label define a2_physhealth_lab  .n "Not in universe", add
label define a2_physhealth_lab  .l "Logical skip", add
label define a2_physhealth_lab  .d "Suppressed for confidentiality", add
cap label values a2_physhealth a2_physhealth_lab
label define a2_relation_lab  8  "There is only one primary adult caregiver in the household for this child"
label define a2_relation_lab  1  "Biological or Adoptive Parent", add
label define a2_relation_lab  2  "Step-parent", add
label define a2_relation_lab  3  "Grandparent", add
label define a2_relation_lab  4  "Foster Parent", add
label define a2_relation_lab  6  "Other: Relative", add
label define a2_relation_lab  7  "Other: Non-Relative", add
label define a2_relation_lab  .m "No valid response", add
label define a2_relation_lab  .n "Not in universe", add
label define a2_relation_lab  .l "Logical skip", add
label define a2_relation_lab  .d "Suppressed for confidentiality", add
cap label values a2_relation a2_relation_lab
label define a2_sex_lab  1  "Male"
label define a2_sex_lab  2  "Female", add
label define a2_sex_lab  .m "No valid response", add
label define a2_sex_lab  .n "Not in universe", add
label define a2_sex_lab  .l "Logical skip", add
label define a2_sex_lab  .d "Suppressed for confidentiality", add
cap label values a2_sex a2_sex_lab
label define ace1_lab  1  "Never"
label define ace1_lab  2  "Rarely", add
label define ace1_lab  3  "Somewhat often", add
label define ace1_lab  4  "Very often", add
label define ace1_lab  .m "No valid response", add
label define ace1_lab  .n "Not in universe", add
label define ace1_lab  .l "Logical skip", add
label define ace1_lab  .d "Suppressed for confidentiality", add
cap label values ace1 ace1_lab
label define ace10_lab  1  "Yes"
label define ace10_lab  2  "No", add
label define ace10_lab  .m "No valid response", add
label define ace10_lab  .n "Not in universe", add
label define ace10_lab  .l "Logical skip", add
label define ace10_lab  .d "Suppressed for confidentiality", add
cap label values ace10 ace10_lab
label define ace11_lab  1  "Yes"
label define ace11_lab  2  "No", add
label define ace11_lab  .m "No valid response", add
label define ace11_lab  .n "Not in universe", add
label define ace11_lab  .l "Logical skip", add
label define ace11_lab  .d "Suppressed for confidentiality", add
cap label values ace11 ace11_lab
label define ace12_lab  1  "Yes"
label define ace12_lab  2  "No", add
label define ace12_lab  .m "No valid response", add
label define ace12_lab  .n "Not in universe", add
label define ace12_lab  .l "Logical skip", add
label define ace12_lab  .d "Suppressed for confidentiality", add
cap label values ace12 ace12_lab
label define ace3_lab  1  "Yes"
label define ace3_lab  2  "No", add
label define ace3_lab  .m "No valid response", add
label define ace3_lab  .n "Not in universe", add
label define ace3_lab  .l "Logical skip", add
label define ace3_lab  .d "Suppressed for confidentiality", add
cap label values ace3 ace3_lab
label define ace4_lab  1  "Yes"
label define ace4_lab  2  "No", add
label define ace4_lab  .m "No valid response", add
label define ace4_lab  .n "Not in universe", add
label define ace4_lab  .l "Logical skip", add
label define ace4_lab  .d "Suppressed for confidentiality", add
cap label values ace4 ace4_lab
label define ace5_lab  1  "Yes"
label define ace5_lab  2  "No", add
label define ace5_lab  .m "No valid response", add
label define ace5_lab  .n "Not in universe", add
label define ace5_lab  .l "Logical skip", add
label define ace5_lab  .d "Suppressed for confidentiality", add
cap label values ace5 ace5_lab
label define ace6_lab  1  "Yes"
label define ace6_lab  2  "No", add
label define ace6_lab  .m "No valid response", add
label define ace6_lab  .n "Not in universe", add
label define ace6_lab  .l "Logical skip", add
label define ace6_lab  .d "Suppressed for confidentiality", add
cap label values ace6 ace6_lab
label define ace7_lab  1  "Yes"
label define ace7_lab  2  "No", add
label define ace7_lab  .m "No valid response", add
label define ace7_lab  .n "Not in universe", add
label define ace7_lab  .l "Logical skip", add
label define ace7_lab  .d "Suppressed for confidentiality", add
cap label values ace7 ace7_lab
label define ace8_lab  1  "Yes"
label define ace8_lab  2  "No", add
label define ace8_lab  .m "No valid response", add
label define ace8_lab  .n "Not in universe", add
label define ace8_lab  .l "Logical skip", add
label define ace8_lab  .d "Suppressed for confidentiality", add
cap label values ace8 ace8_lab
label define ace9_lab  1  "Yes"
label define ace9_lab  2  "No", add
label define ace9_lab  .m "No valid response", add
label define ace9_lab  .n "Not in universe", add
label define ace9_lab  .l "Logical skip", add
label define ace9_lab  .d "Suppressed for confidentiality", add
cap label values ace9 ace9_lab
label define addtreat_lab  1  "Yes"
label define addtreat_lab  2  "No", add
label define addtreat_lab  .m "No valid response", add
label define addtreat_lab  .n "Not in universe", add
label define addtreat_lab  .l "Logical skip", add
label define addtreat_lab  .d "Suppressed for confidentiality", add
cap label values addtreat addtreat_lab
label define agepos4_lab  1  "Only child"
label define agepos4_lab  2  "Oldest child", add
label define agepos4_lab  3  "Second oldest child", add
label define agepos4_lab  4  "Third oldest child", add
label define agepos4_lab  5  "Fourth or greater oldest child", add
label define agepos4_lab  .m "No valid response", add
label define agepos4_lab  .n "Not in universe", add
label define agepos4_lab  .l "Logical skip", add
label define agepos4_lab  .d "Suppressed for confidentiality", add
cap label values agepos4 agepos4_lab
label define allergies_lab  1  "Yes"
label define allergies_lab  2  "No", add
label define allergies_lab  .m "No valid response", add
label define allergies_lab  .n "Not in universe", add
label define allergies_lab  .l "Logical skip", add
label define allergies_lab  .d "Suppressed for confidentiality", add
cap label values allergies allergies_lab
label define allergies_curr_lab  1  "Yes"
label define allergies_curr_lab  2  "No", add
label define allergies_curr_lab  .m "No valid response", add
label define allergies_curr_lab  .n "Not in universe", add
label define allergies_curr_lab  .l "Logical skip", add
label define allergies_curr_lab  .d "Suppressed for confidentiality", add
cap label values allergies_curr allergies_curr_lab
label define allergies_desc_lab  1  "Mild"
label define allergies_desc_lab  4  "Moderate/Severe", add
label define allergies_desc_lab  .m "No valid response", add
label define allergies_desc_lab  .n "Not in universe", add
label define allergies_desc_lab  .l "Logical skip", add
label define allergies_desc_lab  .d "Suppressed for confidentiality", add
cap label values allergies_desc allergies_desc_lab
label define althealth_lab  1  "Yes"
label define althealth_lab  2  "No", add
label define althealth_lab  .m "No valid response", add
label define althealth_lab  .n "Not in universe", add
label define althealth_lab  .l "Logical skip", add
label define althealth_lab  .d "Suppressed for confidentiality", add
cap label values althealth althealth_lab
label define appointment_lab  1  "Yes"
label define appointment_lab  2  "No", add
label define appointment_lab  .m "No valid response", add
label define appointment_lab  .n "Not in universe", add
label define appointment_lab  .l "Logical skip", add
label define appointment_lab  .d "Suppressed for confidentiality", add
cap label values appointment appointment_lab
label define arrangehc_lab  6  "This child does not need health care coordinated on a weekly basis"
label define arrangehc_lab  2  "Less than 1 hour per week", add
label define arrangehc_lab  3  "1-4 hours per week", add
label define arrangehc_lab  4  "5-10 hours per week", add
label define arrangehc_lab  5  "11 or more hours per week", add
label define arrangehc_lab  .m "No valid response", add
label define arrangehc_lab  .n "Not in universe", add
label define arrangehc_lab  .l "Logical skip", add
label define arrangehc_lab  .d "Suppressed for confidentiality", add
cap label values arrangehc arrangehc_lab
label define askquestion_lab  1  "Yes"
label define askquestion_lab  2  "No", add
label define askquestion_lab  .m "No valid response", add
label define askquestion_lab  .n "Not in universe", add
label define askquestion_lab  .l "Logical skip", add
label define askquestion_lab  .d "Suppressed for confidentiality", add
cap label values askquestion askquestion_lab
label define askquestion2_lab  1  "Yes"
label define askquestion2_lab  2  "No", add
label define askquestion2_lab  .m "No valid response", add
label define askquestion2_lab  .n "Not in universe", add
label define askquestion2_lab  .l "Logical skip", add
label define askquestion2_lab  .d "Suppressed for confidentiality", add
cap label values askquestion2 askquestion2_lab
label define athomehc_lab  6  "This child does not need health care provided at home on a weekly basis"
label define athomehc_lab  2  "Less than 1 hour per week", add
label define athomehc_lab  3  "1-4 hours per week", add
label define athomehc_lab  4  "5-10 hours per week", add
label define athomehc_lab  5  "11 or more hours per week", add
label define athomehc_lab  .m "No valid response", add
label define athomehc_lab  .n "Not in universe", add
label define athomehc_lab  .l "Logical skip", add
label define athomehc_lab  .d "Suppressed for confidentiality", add
cap label values athomehc athomehc_lab
label define autismmed_lab  1  "Yes"
label define autismmed_lab  2  "No", add
label define autismmed_lab  .m "No valid response", add
label define autismmed_lab  .n "Not in universe", add
label define autismmed_lab  .l "Logical skip", add
label define autismmed_lab  .d "Suppressed for confidentiality", add
cap label values autismmed autismmed_lab
label define autismtreat_lab  1  "Yes"
label define autismtreat_lab  2  "No", add
label define autismtreat_lab  .m "No valid response", add
label define autismtreat_lab  .n "Not in universe", add
label define autismtreat_lab  .l "Logical skip", add
label define autismtreat_lab  .d "Suppressed for confidentiality", add
cap label values autismtreat autismtreat_lab
label define autoimmune_lab  1  "Yes"
label define autoimmune_lab  2  "No", add
label define autoimmune_lab  .m "No valid response", add
label define autoimmune_lab  .n "Not in universe", add
label define autoimmune_lab  .l "Logical skip", add
label define autoimmune_lab  .d "Suppressed for confidentiality", add
cap label values autoimmune autoimmune_lab
label define autoimmune_desc_lab  1  "Mild"
label define autoimmune_desc_lab  2  "Moderate", add
label define autoimmune_desc_lab  3  "Severe", add
label define autoimmune_desc_lab  .m "No valid response", add
label define autoimmune_desc_lab  .n "Not in universe", add
label define autoimmune_desc_lab  .l "Logical skip", add
label define autoimmune_desc_lab  .d "Suppressed for confidentiality", add
cap label values autoimmune_desc autoimmune_desc_lab
label define available_lab  1  "Yes"
label define available_lab  2  "No", add
label define available_lab  .m "No valid response", add
label define available_lab  .n "Not in universe", add
label define available_lab  .l "Logical skip", add
label define available_lab  .d "Suppressed for confidentiality", add
cap label values available available_lab
label define avoidchg_lab  1  "Yes"
label define avoidchg_lab  2  "No", add
label define avoidchg_lab  .m "No valid response", add
label define avoidchg_lab  .n "Not in universe", add
label define avoidchg_lab  .l "Logical skip", add
label define avoidchg_lab  .d "Suppressed for confidentiality", add
cap label values avoidchg avoidchg_lab
label define bedtime_lab  1  "Always"
label define bedtime_lab  2  "Usually", add
label define bedtime_lab  3  "Sometimes", add
label define bedtime_lab  4  "Rarely", add
label define bedtime_lab  5  "Never", add
label define bedtime_lab  .m "No valid response", add
label define bedtime_lab  .n "Not in universe", add
label define bedtime_lab  .l "Logical skip", add
label define bedtime_lab  .d "Suppressed for confidentiality", add
cap label values bedtime bedtime_lab
label define bestforchild_lab  1  "Always"
label define bestforchild_lab  2  "Usually", add
label define bestforchild_lab  3  "Sometimes", add
label define bestforchild_lab  4  "Never", add
label define bestforchild_lab  .m "No valid response", add
label define bestforchild_lab  .n "Not in universe", add
label define bestforchild_lab  .l "Logical skip", add
label define bestforchild_lab  .d "Suppressed for confidentiality", add
cap label values bestforchild bestforchild_lab
label define birth_mo_lab  1  "January"
label define birth_mo_lab  2  "February", add
label define birth_mo_lab  3  "March", add
label define birth_mo_lab  4  "April", add
label define birth_mo_lab  5  "May", add
label define birth_mo_lab  6  "June", add
label define birth_mo_lab  7  "July", add
label define birth_mo_lab  8  "August", add
label define birth_mo_lab  9  "September", add
label define birth_mo_lab  .m "No valid response", add
label define birth_mo_lab  .n "Not in universe", add
label define birth_mo_lab  .l "Logical skip", add
label define birth_mo_lab  .d "Suppressed for confidentiality", add
cap label values birth_mo birth_mo_lab
label define birth_yr_lab  .m "No valid response"
label define birth_yr_lab  .n "Not in universe", add
label define birth_yr_lab  .l "Logical skip", add
label define birth_yr_lab  .d "Suppressed for confidentiality", add
cap label values birth_yr birth_yr_lab
label define birth_yr_f_lab  0  "No data quality concerns"
label define birth_yr_f_lab  1  "Child age, SC_AGE_YEARS, and birth year,  BIRTH_YR, are inconsistent", add
label define birth_yr_f_lab  2  "Reported birth year, BIRTH_YR, is missing", add
label define birth_yr_f_lab  3  "Reported birth month, BIRTH_MO, is missing", add
label define birth_yr_f_lab  .m "No valid response", add
label define birth_yr_f_lab  .n "Not in universe", add
label define birth_yr_f_lab  .l "Logical skip", add
label define birth_yr_f_lab  .d "Suppressed for confidentiality", add
cap label values birth_yr_f birth_yr_f_lab
label define birthwt_lab  1  "Very low birth weight (less than 1,500g)"
label define birthwt_lab  2  "Low birth weight (less than 2,500g)", add
label define birthwt_lab  3  "Not low birth weight", add
label define birthwt_lab  .m "No valid response", add
label define birthwt_lab  .n "Not in universe", add
label define birthwt_lab  .l "Logical skip", add
label define birthwt_lab  .d "Suppressed for confidentiality", add
cap label values birthwt birthwt_lab
label define birthwt_l_lab  1  "Yes"
label define birthwt_l_lab  2  "No", add
label define birthwt_l_lab  .m "No valid response", add
label define birthwt_l_lab  .n "Not in universe", add
label define birthwt_l_lab  .l "Logical skip", add
label define birthwt_l_lab  .d "Suppressed for confidentiality", add
cap label values birthwt_l birthwt_l_lab
label define birthwt_oz_s_lab  .m "No valid response"
label define birthwt_oz_s_lab  .n "Not in universe", add
label define birthwt_oz_s_lab  .l "Logical skip", add
label define birthwt_oz_s_lab  .d "Suppressed for confidentiality", add
cap label values birthwt_oz_s birthwt_oz_s_lab
label define birthwt_vl_lab  1  "Yes"
label define birthwt_vl_lab  2  "No", add
label define birthwt_vl_lab  .m "No valid response", add
label define birthwt_vl_lab  .n "Not in universe", add
label define birthwt_vl_lab  .l "Logical skip", add
label define birthwt_vl_lab  .d "Suppressed for confidentiality", add
cap label values birthwt_vl birthwt_vl_lab
label define blindness_lab  1  "Yes"
label define blindness_lab  2  "No", add
label define blindness_lab  .m "No valid response", add
label define blindness_lab  .n "Not in universe", add
label define blindness_lab  .l "Logical skip", add
label define blindness_lab  .d "Suppressed for confidentiality", add
cap label values blindness blindness_lab
label define blood_lab  1  "Yes"
label define blood_lab  2  "No", add
label define blood_lab  .m "No valid response", add
label define blood_lab  .n "Not in universe", add
label define blood_lab  .l "Logical skip", add
label define blood_lab  .d "Suppressed for confidentiality", add
cap label values blood blood_lab
label define blood_desc_lab  1  "Mild"
label define blood_desc_lab  2  "Moderate", add
label define blood_desc_lab  3  "Severe", add
label define blood_desc_lab  .m "No valid response", add
label define blood_desc_lab  .n "Not in universe", add
label define blood_desc_lab  .l "Logical skip", add
label define blood_desc_lab  .d "Suppressed for confidentiality", add
cap label values blood_desc blood_desc_lab
label define blood_other_lab  1  "Yes"
label define blood_other_lab  2  "No", add
label define blood_other_lab  .m "No valid response", add
label define blood_other_lab  .n "Not in universe", add
label define blood_other_lab  .l "Logical skip", add
label define blood_other_lab  .d "Suppressed for confidentiality", add
cap label values blood_other blood_other_lab
label define blood_screen_lab  1  "Yes"
label define blood_screen_lab  2  "No", add
label define blood_screen_lab  .m "No valid response", add
label define blood_screen_lab  .n "Not in universe", add
label define blood_screen_lab  .l "Logical skip", add
label define blood_screen_lab  .d "Suppressed for confidentiality", add
cap label values blood_screen blood_screen_lab
label define bmiclass_lab  1  "Less than the 5th percentile"
label define bmiclass_lab  2  "5th percentile to less than the 85th percentile", add
label define bmiclass_lab  3  "85th percentile to less than the 95th percentile", add
label define bmiclass_lab  4  "Equal to or greater than the 95th percentile", add
label define bmiclass_lab  .m "No valid response", add
label define bmiclass_lab  .n "Not in universe", add
label define bmiclass_lab  .l "Logical skip", add
label define bmiclass_lab  .d "Suppressed for confidentiality", add
cap label values bmiclass bmiclass_lab
label define bodyimage_lab  1  "Very much"
label define bodyimage_lab  2  "Somewhat", add
label define bodyimage_lab  3  "Not at all", add
label define bodyimage_lab  .m "No valid response", add
label define bodyimage_lab  .n "Not in universe", add
label define bodyimage_lab  .l "Logical skip", add
label define bodyimage_lab  .d "Suppressed for confidentiality", add
cap label values bodyimage bodyimage_lab
label define bornusa_lab  1  "Yes"
label define bornusa_lab  2  "No", add
label define bornusa_lab  .m "No valid response", add
label define bornusa_lab  .n "Not in universe", add
label define bornusa_lab  .l "Logical skip", add
label define bornusa_lab  .d "Suppressed for confidentiality", add
cap label values bornusa bornusa_lab
label define bounceaball_lab  1  "This child cannot bounce a ball"
label define bounceaball_lab  2  "Not well", add
label define bounceaball_lab  3  "Somewhat well", add
label define bounceaball_lab  4  "Very well", add
label define bounceaball_lab  .m "No valid response", add
label define bounceaball_lab  .n "Not in universe", add
label define bounceaball_lab  .l "Logical skip", add
label define bounceaball_lab  .d "Suppressed for confidentiality", add
cap label values bounceaball bounceaball_lab
label define breastfedend_day_s_lab  .m "No valid response"
label define breastfedend_day_s_lab  .n "Not in universe", add
label define breastfedend_day_s_lab  .l "Logical skip", add
label define breastfedend_day_s_lab  .d "Suppressed for confidentiality", add
cap label values breastfedend_day_s breastfedend_day_s_lab
label define breastfedend_mo_s_lab  .m "No valid response"
label define breastfedend_mo_s_lab  .n "Not in universe", add
label define breastfedend_mo_s_lab  .l "Logical skip", add
label define breastfedend_mo_s_lab  .d "Suppressed for confidentiality", add
cap label values breastfedend_mo_s breastfedend_mo_s_lab
label define breastfedend_wk_s_lab  .m "No valid response"
label define breastfedend_wk_s_lab  .n "Not in universe", add
label define breastfedend_wk_s_lab  .l "Logical skip", add
label define breastfedend_wk_s_lab  .d "Suppressed for confidentiality", add
cap label values breastfedend_wk_s breastfedend_wk_s_lab
label define breathing_lab  1  "Yes"
label define breathing_lab  2  "No", add
label define breathing_lab  .m "No valid response", add
label define breathing_lab  .n "Not in universe", add
label define breathing_lab  .l "Logical skip", add
label define breathing_lab  .d "Suppressed for confidentiality", add
cap label values breathing breathing_lab
label define bullied_r_lab  1  "Never (in the past 12 months)"
label define bullied_r_lab  2  "1-2 times (in the past 12 months)", add
label define bullied_r_lab  3  "1-2 times per month", add
label define bullied_r_lab  4  "1-2 times per week", add
label define bullied_r_lab  5  "Almost every day", add
label define bullied_r_lab  .m "No valid response", add
label define bullied_r_lab  .n "Not in universe", add
label define bullied_r_lab  .l "Logical skip", add
label define bullied_r_lab  .d "Suppressed for confidentiality", add
cap label values bullied_r bullied_r_lab
label define bully_lab  1  "Never (in the past 12 months)"
label define bully_lab  2  "1-2 times (in the past 12 months)", add
label define bully_lab  3  "1-2 times per month", add
label define bully_lab  4  "1-2 times per week", add
label define bully_lab  5  "Almost every day", add
label define bully_lab  .m "No valid response", add
label define bully_lab  .n "Not in universe", add
label define bully_lab  .l "Logical skip", add
label define bully_lab  .d "Suppressed for confidentiality", add
cap label values bully bully_lab
label define c4q04_lab  1  "Never"
label define c4q04_lab  2  "Sometimes", add
label define c4q04_lab  3  "Usually", add
label define c4q04_lab  4  "Always", add
label define c4q04_lab  .m "No valid response", add
label define c4q04_lab  .n "Not in universe", add
label define c4q04_lab  .l "Logical skip", add
label define c4q04_lab  .d "Suppressed for confidentiality", add
cap label values c4q04 c4q04_lab
label define calmdown_r_lab  1  "Always"
label define calmdown_r_lab  2  "Most of the time", add
label define calmdown_r_lab  3  "About half the time", add
label define calmdown_r_lab  4  "Sometimes", add
label define calmdown_r_lab  5  "Never", add
label define calmdown_r_lab  .m "No valid response", add
label define calmdown_r_lab  .n "Not in universe", add
label define calmdown_r_lab  .l "Logical skip", add
label define calmdown_r_lab  .d "Suppressed for confidentiality", add
cap label values calmdown_r calmdown_r_lab
label define cavities_lab  1  "Yes"
label define cavities_lab  2  "No", add
label define cavities_lab  .m "No valid response", add
label define cavities_lab  .n "Not in universe", add
label define cavities_lab  .l "Logical skip", add
label define cavities_lab  .d "Suppressed for confidentiality", add
cap label values cavities cavities_lab
label define cbsafp_yn_lab  1  "Located within a CBSA"
label define cbsafp_yn_lab  2  "Located outside a CBSA", add
label define cbsafp_yn_lab  .m "No valid response", add
label define cbsafp_yn_lab  .n "Not in universe", add
label define cbsafp_yn_lab  .l "Logical skip", add
label define cbsafp_yn_lab  .d "Suppressed for confidentiality", add
cap label values cbsafp_yn cbsafp_yn_lab
label define cerpals_desc_lab  1  "Mild"
label define cerpals_desc_lab  2  "Moderate", add
label define cerpals_desc_lab  3  "Severe", add
label define cerpals_desc_lab  .m "No valid response", add
label define cerpals_desc_lab  .n "Not in universe", add
label define cerpals_desc_lab  .l "Logical skip", add
label define cerpals_desc_lab  .d "Suppressed for confidentiality", add
cap label values cerpals_desc cerpals_desc_lab
label define changeage_lab  1  "Yes"
label define changeage_lab  2  "No", add
label define changeage_lab  3  "Don't Know", add
label define changeage_lab  .m "No valid response", add
label define changeage_lab  .n "Not in universe", add
label define changeage_lab  .l "Logical skip", add
label define changeage_lab  .d "Suppressed for confidentiality", add
cap label values changeage changeage_lab
label define clearexp_lab  1  "Always"
label define clearexp_lab  2  "Most of the time", add
label define clearexp_lab  3  "About half the time", add
label define clearexp_lab  4  "Sometimes", add
label define clearexp_lab  5  "Never", add
label define clearexp_lab  .m "No valid response", add
label define clearexp_lab  .n "Not in universe", add
label define clearexp_lab  .l "Logical skip", add
label define clearexp_lab  .d "Suppressed for confidentiality", add
cap label values clearexp clearexp_lab
label define concussion_lab  1  "Yes"
label define concussion_lab  2  "No", add
label define concussion_lab  .m "No valid response", add
label define concussion_lab  .n "Not in universe", add
label define concussion_lab  .l "Logical skip", add
label define concussion_lab  .d "Suppressed for confidentiality", add
cap label values concussion concussion_lab
label define confirminjury_lab  1  "Yes"
label define confirminjury_lab  2  "No", add
label define confirminjury_lab  .m "No valid response", add
label define confirminjury_lab  .n "Not in universe", add
label define confirminjury_lab  .l "Logical skip", add
label define confirminjury_lab  .d "Suppressed for confidentiality", add
cap label values confirminjury confirminjury_lab
label define coordination_lab  1  "Yes"
label define coordination_lab  2  "No", add
label define coordination_lab  .m "No valid response", add
label define coordination_lab  .n "Not in universe", add
label define coordination_lab  .l "Logical skip", add
label define coordination_lab  .d "Suppressed for confidentiality", add
cap label values coordination coordination_lab
label define countto_r_lab  1  "This child cannot count"
label define countto_r_lab  2  "Up to five", add
label define countto_r_lab  3  "Up to ten", add
label define countto_r_lab  4  "Up to 20", add
label define countto_r_lab  5  "Up to 30 or more", add
label define countto_r_lab  .m "No valid response", add
label define countto_r_lab  .n "Not in universe", add
label define countto_r_lab  .l "Logical skip", add
label define countto_r_lab  .d "Suppressed for confidentiality", add
cap label values countto_r countto_r_lab
label define covidarrange_lab  1  "Yes"
label define covidarrange_lab  2  "No", add
label define covidarrange_lab  .m "No valid response", add
label define covidarrange_lab  .n "Not in universe", add
label define covidarrange_lab  .l "Logical skip", add
label define covidarrange_lab  .d "Suppressed for confidentiality", add
cap label values covidarrange covidarrange_lab
label define covidcheckups_lab  1  "Yes"
label define covidcheckups_lab  2  "No", add
label define covidcheckups_lab  .m "No valid response", add
label define covidcheckups_lab  .n "Not in universe", add
label define covidcheckups_lab  .l "Logical skip", add
label define covidcheckups_lab  .d "Suppressed for confidentiality", add
cap label values covidcheckups covidcheckups_lab
label define currcov_lab  1  "Yes"
label define currcov_lab  2  "No", add
label define currcov_lab  .m "No valid response", add
label define currcov_lab  .n "Not in universe", add
label define currcov_lab  .l "Logical skip", add
label define currcov_lab  .d "Suppressed for confidentiality", add
cap label values currcov currcov_lab
label define currins_lab  1  "Currently insured (does not include Indian Health Service or a religious health share)"
label define currins_lab  2  "Currently uninsured, or only insured through Indian Health Service or a religious health share", add
label define currins_lab  .m "No valid response", add
label define currins_lab  .n "Not in universe", add
label define currins_lab  .l "Logical skip", add
label define currins_lab  .d "Suppressed for confidentiality", add
cap label values currins currins_lab
label define cuthours_lab  1  "Yes"
label define cuthours_lab  2  "No", add
label define cuthours_lab  .m "No valid response", add
label define cuthours_lab  .n "Not in universe", add
label define cuthours_lab  .l "Logical skip", add
label define cuthours_lab  .d "Suppressed for confidentiality", add
cap label values cuthours cuthours_lab
label define cystfib_lab  1  "Yes"
label define cystfib_lab  2  "No", add
label define cystfib_lab  .m "No valid response", add
label define cystfib_lab  .n "Not in universe", add
label define cystfib_lab  .l "Logical skip", add
label define cystfib_lab  .d "Suppressed for confidentiality", add
cap label values cystfib cystfib_lab
label define cystfib_screen_lab  1  "Yes"
label define cystfib_screen_lab  2  "No", add
label define cystfib_screen_lab  .m "No valid response", add
label define cystfib_screen_lab  .n "Not in universe", add
label define cystfib_screen_lab  .l "Logical skip", add
label define cystfib_screen_lab  .d "Suppressed for confidentiality", add
cap label values cystfib_screen cystfib_screen_lab
label define decisions_lab  1  "Yes"
label define decisions_lab  2  "No", add
label define decisions_lab  .m "No valid response", add
label define decisions_lab  .n "Not in universe", add
label define decisions_lab  .l "Logical skip", add
label define decisions_lab  .d "Suppressed for confidentiality", add
cap label values decisions decisions_lab
label define dentalserv1_lab  1  "selected"
label define dentalserv1_lab  2  "not selected", add
label define dentalserv1_lab  .m "No valid response", add
label define dentalserv1_lab  .n "Not in universe", add
label define dentalserv1_lab  .l "Logical skip", add
label define dentalserv1_lab  .d "Suppressed for confidentiality", add
cap label values dentalserv1 dentalserv1_lab
label define dentalserv2_lab  1  "selected"
label define dentalserv2_lab  2  "not selected", add
label define dentalserv2_lab  .m "No valid response", add
label define dentalserv2_lab  .n "Not in universe", add
label define dentalserv2_lab  .l "Logical skip", add
label define dentalserv2_lab  .d "Suppressed for confidentiality", add
cap label values dentalserv2 dentalserv2_lab
label define dentalserv3_lab  1  "selected"
label define dentalserv3_lab  2  "not selected", add
label define dentalserv3_lab  .m "No valid response", add
label define dentalserv3_lab  .n "Not in universe", add
label define dentalserv3_lab  .l "Logical skip", add
label define dentalserv3_lab  .d "Suppressed for confidentiality", add
cap label values dentalserv3 dentalserv3_lab
label define dentalserv4_lab  1  "selected"
label define dentalserv4_lab  2  "not selected", add
label define dentalserv4_lab  .m "No valid response", add
label define dentalserv4_lab  .n "Not in universe", add
label define dentalserv4_lab  .l "Logical skip", add
label define dentalserv4_lab  .d "Suppressed for confidentiality", add
cap label values dentalserv4 dentalserv4_lab
label define dentalserv5_lab  1  "selected"
label define dentalserv5_lab  2  "not selected", add
label define dentalserv5_lab  .m "No valid response", add
label define dentalserv5_lab  .n "Not in universe", add
label define dentalserv5_lab  .l "Logical skip", add
label define dentalserv5_lab  .d "Suppressed for confidentiality", add
cap label values dentalserv5 dentalserv5_lab
label define dentalserv6_lab  1  "selected"
label define dentalserv6_lab  2  "not selected", add
label define dentalserv6_lab  .m "No valid response", add
label define dentalserv6_lab  .n "Not in universe", add
label define dentalserv6_lab  .l "Logical skip", add
label define dentalserv6_lab  .d "Suppressed for confidentiality", add
cap label values dentalserv6 dentalserv6_lab
label define dentalserv7_lab  1  "selected"
label define dentalserv7_lab  2  "not selected", add
label define dentalserv7_lab  .m "No valid response", add
label define dentalserv7_lab  .n "Not in universe", add
label define dentalserv7_lab  .l "Logical skip", add
label define dentalserv7_lab  .d "Suppressed for confidentiality", add
cap label values dentalserv7 dentalserv7_lab
label define dentistvisit_lab  1  "No preventive visits in past 12 months"
label define dentistvisit_lab  2  "Yes, 1 visit", add
label define dentistvisit_lab  3  "Yes, 2 or more visits", add
label define dentistvisit_lab  .m "No valid response", add
label define dentistvisit_lab  .n "Not in universe", add
label define dentistvisit_lab  .l "Logical skip", add
label define dentistvisit_lab  .d "Suppressed for confidentiality", add
cap label values dentistvisit dentistvisit_lab
label define diabetes_lab  1  "Yes"
label define diabetes_lab  2  "No", add
label define diabetes_lab  .m "No valid response", add
label define diabetes_lab  .n "Not in universe", add
label define diabetes_lab  .l "Logical skip", add
label define diabetes_lab  .d "Suppressed for confidentiality", add
cap label values diabetes diabetes_lab
label define diabetes_curr_lab  1  "Yes"
label define diabetes_curr_lab  2  "No", add
label define diabetes_curr_lab  .m "No valid response", add
label define diabetes_curr_lab  .n "Not in universe", add
label define diabetes_curr_lab  .l "Logical skip", add
label define diabetes_curr_lab  .d "Suppressed for confidentiality", add
cap label values diabetes_curr diabetes_curr_lab
label define diabetes_desc_lab  1  "Mild"
label define diabetes_desc_lab  4  "Moderate/Severe", add
label define diabetes_desc_lab  .m "No valid response", add
label define diabetes_desc_lab  .n "Not in universe", add
label define diabetes_desc_lab  .l "Logical skip", add
label define diabetes_desc_lab  .d "Suppressed for confidentiality", add
cap label values diabetes_desc diabetes_desc_lab
label define directions_lab  1  "Yes"
label define directions_lab  2  "No", add
label define directions_lab  .m "No valid response", add
label define directions_lab  .n "Not in universe", add
label define directions_lab  .l "Logical skip", add
label define directions_lab  .d "Suppressed for confidentiality", add
cap label values directions directions_lab
label define directions2_lab  1  "Yes"
label define directions2_lab  2  "No", add
label define directions2_lab  .m "No valid response", add
label define directions2_lab  .n "Not in universe", add
label define directions2_lab  .l "Logical skip", add
label define directions2_lab  .d "Suppressed for confidentiality", add
cap label values directions2 directions2_lab
label define discussopt_lab  1  "Always"
label define discussopt_lab  2  "Usually", add
label define discussopt_lab  3  "Sometimes", add
label define discussopt_lab  4  "Never", add
label define discussopt_lab  .m "No valid response", add
label define discussopt_lab  .n "Not in universe", add
label define discussopt_lab  .l "Logical skip", add
label define discussopt_lab  .d "Suppressed for confidentiality", add
cap label values discussopt discussopt_lab
label define distracted_lab  1  "Always"
label define distracted_lab  2  "Most of the time", add
label define distracted_lab  3  "About half the time", add
label define distracted_lab  4  "Sometimes", add
label define distracted_lab  5  "Never", add
label define distracted_lab  .m "No valid response", add
label define distracted_lab  .n "Not in universe", add
label define distracted_lab  .l "Logical skip", add
label define distracted_lab  .d "Suppressed for confidentiality", add
cap label values distracted distracted_lab
label define docprivate_lab  1  "Yes"
label define docprivate_lab  2  "No", add
label define docprivate_lab  .m "No valid response", add
label define docprivate_lab  .n "Not in universe", add
label define docprivate_lab  .l "Logical skip", add
label define docprivate_lab  .d "Suppressed for confidentiality", add
cap label values docprivate docprivate_lab
label define docroom_lab  1  "Less than 10 minutes"
label define docroom_lab  2  "10-20 minutes", add
label define docroom_lab  3  "More than 20 minutes", add
label define docroom_lab  .m "No valid response", add
label define docroom_lab  .n "Not in universe", add
label define docroom_lab  .l "Logical skip", add
label define docroom_lab  .d "Suppressed for confidentiality", add
cap label values docroom docroom_lab
label define downsyn_lab  1  "Yes"
label define downsyn_lab  2  "No", add
label define downsyn_lab  .m "No valid response", add
label define downsyn_lab  .n "Not in universe", add
label define downsyn_lab  .l "Logical skip", add
label define downsyn_lab  .d "Suppressed for confidentiality", add
cap label values downsyn downsyn_lab
label define drawacircle_lab  1  "This child cannot draw a circle"
label define drawacircle_lab  2  "Not well", add
label define drawacircle_lab  3  "Somewhat well", add
label define drawacircle_lab  4  "Very well", add
label define drawacircle_lab  .m "No valid response", add
label define drawacircle_lab  .n "Not in universe", add
label define drawacircle_lab  .l "Logical skip", add
label define drawacircle_lab  .d "Suppressed for confidentiality", add
cap label values drawacircle drawacircle_lab
label define drawaface_lab  1  "This child cannot draw a face"
label define drawaface_lab  2  "Not well", add
label define drawaface_lab  3  "Somewhat well", add
label define drawaface_lab  4  "Very well", add
label define drawaface_lab  .m "No valid response", add
label define drawaface_lab  .n "Not in universe", add
label define drawaface_lab  .l "Logical skip", add
label define drawaface_lab  .d "Suppressed for confidentiality", add
cap label values drawaface drawaface_lab
label define drawaperson_lab  1  "This child cannot draw a person"
label define drawaperson_lab  2  "Not well", add
label define drawaperson_lab  3  "Somewhat well", add
label define drawaperson_lab  4  "Very well", add
label define drawaperson_lab  .m "No valid response", add
label define drawaperson_lab  .n "Not in universe", add
label define drawaperson_lab  .l "Logical skip", add
label define drawaperson_lab  .d "Suppressed for confidentiality", add
cap label values drawaperson drawaperson_lab
label define dressing_lab  1  "Yes"
label define dressing_lab  2  "No", add
label define dressing_lab  .m "No valid response", add
label define dressing_lab  .n "Not in universe", add
label define dressing_lab  .l "Logical skip", add
label define dressing_lab  .d "Suppressed for confidentiality", add
cap label values dressing dressing_lab
label define ebtcards_lab  1  "Yes"
label define ebtcards_lab  2  "No", add
label define ebtcards_lab  .m "No valid response", add
label define ebtcards_lab  .n "Not in universe", add
label define ebtcards_lab  .l "Logical skip", add
label define ebtcards_lab  .d "Suppressed for confidentiality", add
cap label values ebtcards ebtcards_lab
label define emosupadv_lab  1  "Yes"
label define emosupadv_lab  2  "No", add
label define emosupadv_lab  .m "No valid response", add
label define emosupadv_lab  .n "Not in universe", add
label define emosupadv_lab  .l "Logical skip", add
label define emosupadv_lab  .d "Suppressed for confidentiality", add
cap label values emosupadv emosupadv_lab
label define emosupfam_lab  1  "Yes"
label define emosupfam_lab  2  "No", add
label define emosupfam_lab  .m "No valid response", add
label define emosupfam_lab  .n "Not in universe", add
label define emosupfam_lab  .l "Logical skip", add
label define emosupfam_lab  .d "Suppressed for confidentiality", add
cap label values emosupfam emosupfam_lab
label define emosuphcp_lab  1  "Yes"
label define emosuphcp_lab  2  "No", add
label define emosuphcp_lab  .m "No valid response", add
label define emosuphcp_lab  .n "Not in universe", add
label define emosuphcp_lab  .l "Logical skip", add
label define emosuphcp_lab  .d "Suppressed for confidentiality", add
cap label values emosuphcp emosuphcp_lab
label define emosupmhp_lab  1  "Yes"
label define emosupmhp_lab  2  "No", add
label define emosupmhp_lab  .m "No valid response", add
label define emosupmhp_lab  .n "Not in universe", add
label define emosupmhp_lab  .l "Logical skip", add
label define emosupmhp_lab  .d "Suppressed for confidentiality", add
cap label values emosupmhp emosupmhp_lab
label define emosupoth_lab  1  "Yes"
label define emosupoth_lab  2  "No", add
label define emosupoth_lab  .m "No valid response", add
label define emosupoth_lab  .n "Not in universe", add
label define emosupoth_lab  .l "Logical skip", add
label define emosupoth_lab  .d "Suppressed for confidentiality", add
cap label values emosupoth emosupoth_lab
label define emosuppeer_lab  1  "Yes"
label define emosuppeer_lab  2  "No", add
label define emosuppeer_lab  .m "No valid response", add
label define emosuppeer_lab  .n "Not in universe", add
label define emosuppeer_lab  .l "Logical skip", add
label define emosuppeer_lab  .d "Suppressed for confidentiality", add
cap label values emosuppeer emosuppeer_lab
label define emosupspo_lab  1  "Yes"
label define emosupspo_lab  2  "No", add
label define emosupspo_lab  .m "No valid response", add
label define emosupspo_lab  .n "Not in universe", add
label define emosupspo_lab  .l "Logical skip", add
label define emosupspo_lab  .d "Suppressed for confidentiality", add
cap label values emosupspo emosupspo_lab
label define emosupwor_lab  1  "Yes"
label define emosupwor_lab  2  "No", add
label define emosupwor_lab  .m "No valid response", add
label define emosupwor_lab  .n "Not in universe", add
label define emosupwor_lab  .l "Logical skip", add
label define emosupwor_lab  .d "Suppressed for confidentiality", add
cap label values emosupwor emosupwor_lab
label define engage_binge_lab  1  "Yes"
label define engage_binge_lab  2  "No", add
label define engage_binge_lab  .m "No valid response", add
label define engage_binge_lab  .n "Not in universe", add
label define engage_binge_lab  .l "Logical skip", add
label define engage_binge_lab  .d "Suppressed for confidentiality", add
cap label values engage_binge engage_binge_lab
label define engage_exercise_lab  1  "Yes"
label define engage_exercise_lab  2  "No", add
label define engage_exercise_lab  .m "No valid response", add
label define engage_exercise_lab  .n "Not in universe", add
label define engage_exercise_lab  .l "Logical skip", add
label define engage_exercise_lab  .d "Suppressed for confidentiality", add
cap label values engage_exercise engage_exercise_lab
label define engage_fast_lab  1  "Yes"
label define engage_fast_lab  2  "No", add
label define engage_fast_lab  .m "No valid response", add
label define engage_fast_lab  .n "Not in universe", add
label define engage_fast_lab  .l "Logical skip", add
label define engage_fast_lab  .d "Suppressed for confidentiality", add
cap label values engage_fast engage_fast_lab
label define engage_interest_lab  1  "Yes"
label define engage_interest_lab  2  "No", add
label define engage_interest_lab  .m "No valid response", add
label define engage_interest_lab  .n "Not in universe", add
label define engage_interest_lab  .l "Logical skip", add
label define engage_interest_lab  .d "Suppressed for confidentiality", add
cap label values engage_interest engage_interest_lab
label define engage_noeat_lab  1  "Yes"
label define engage_noeat_lab  2  "No", add
label define engage_noeat_lab  .m "No valid response", add
label define engage_noeat_lab  .n "Not in universe", add
label define engage_noeat_lab  .l "Logical skip", add
label define engage_noeat_lab  .d "Suppressed for confidentiality", add
cap label values engage_noeat engage_noeat_lab
label define engage_picky_lab  1  "Yes"
label define engage_picky_lab  2  "No", add
label define engage_picky_lab  .m "No valid response", add
label define engage_picky_lab  .n "Not in universe", add
label define engage_picky_lab  .l "Logical skip", add
label define engage_picky_lab  .d "Suppressed for confidentiality", add
cap label values engage_picky engage_picky_lab
label define engage_pills_lab  1  "Yes"
label define engage_pills_lab  2  "No", add
label define engage_pills_lab  .m "No valid response", add
label define engage_pills_lab  .n "Not in universe", add
label define engage_pills_lab  .l "Logical skip", add
label define engage_pills_lab  .d "Suppressed for confidentiality", add
cap label values engage_pills engage_pills_lab
label define engage_purg_lab  1  "Yes"
label define engage_purg_lab  2  "No", add
label define engage_purg_lab  .m "No valid response", add
label define engage_purg_lab  .n "Not in universe", add
label define engage_purg_lab  .l "Logical skip", add
label define engage_purg_lab  .d "Suppressed for confidentiality", add
cap label values engage_purg engage_purg_lab
label define engageconcern_lab  1  "Very much"
label define engageconcern_lab  2  "Somewhat", add
label define engageconcern_lab  3  "Not at all", add
label define engageconcern_lab  .m "No valid response", add
label define engageconcern_lab  .n "Not in universe", add
label define engageconcern_lab  .l "Logical skip", add
label define engageconcern_lab  .d "Suppressed for confidentiality", add
cap label values engageconcern engageconcern_lab
label define errandalone_lab  1  "Yes"
label define errandalone_lab  2  "No", add
label define errandalone_lab  .m "No valid response", add
label define errandalone_lab  .n "Not in universe", add
label define errandalone_lab  .l "Logical skip", add
label define errandalone_lab  .d "Suppressed for confidentiality", add
cap label values errandalone errandalone_lab
label define evalfasd_lab  1  "Yes"
label define evalfasd_lab  2  "No", add
label define evalfasd_lab  3  "Don't Know", add
label define evalfasd_lab  .m "No valid response", add
label define evalfasd_lab  .n "Not in universe", add
label define evalfasd_lab  .l "Logical skip", add
label define evalfasd_lab  .d "Suppressed for confidentiality", add
cap label values evalfasd evalfasd_lab
label define everhomeless_lab  1  "Yes"
label define everhomeless_lab  2  "No", add
label define everhomeless_lab  3  "Don't Know", add
label define everhomeless_lab  .m "No valid response", add
label define everhomeless_lab  .n "Not in universe", add
label define everhomeless_lab  .l "Logical skip", add
label define everhomeless_lab  .d "Suppressed for confidentiality", add
cap label values everhomeless everhomeless_lab
label define eyecare1_lab  1  "selected"
label define eyecare1_lab  2  "not selected", add
label define eyecare1_lab  .m "No valid response", add
label define eyecare1_lab  .n "Not in universe", add
label define eyecare1_lab  .l "Logical skip", add
label define eyecare1_lab  .d "Suppressed for confidentiality", add
cap label values eyecare1 eyecare1_lab
label define eyecare2_lab  1  "selected"
label define eyecare2_lab  2  "not selected", add
label define eyecare2_lab  .m "No valid response", add
label define eyecare2_lab  .n "Not in universe", add
label define eyecare2_lab  .l "Logical skip", add
label define eyecare2_lab  .d "Suppressed for confidentiality", add
cap label values eyecare2 eyecare2_lab
label define eyecare3_lab  1  "selected"
label define eyecare3_lab  2  "not selected", add
label define eyecare3_lab  .m "No valid response", add
label define eyecare3_lab  .n "Not in universe", add
label define eyecare3_lab  .l "Logical skip", add
label define eyecare3_lab  .d "Suppressed for confidentiality", add
cap label values eyecare3 eyecare3_lab
label define eyecare4_lab  1  "selected"
label define eyecare4_lab  2  "not selected", add
label define eyecare4_lab  .m "No valid response", add
label define eyecare4_lab  .n "Not in universe", add
label define eyecare4_lab  .l "Logical skip", add
label define eyecare4_lab  .d "Suppressed for confidentiality", add
cap label values eyecare4 eyecare4_lab
label define eyedoctor_lab  1  "Yes"
label define eyedoctor_lab  2  "No", add
label define eyedoctor_lab  .m "No valid response", add
label define eyedoctor_lab  .n "Not in universe", add
label define eyedoctor_lab  .l "Logical skip", add
label define eyedoctor_lab  .d "Suppressed for confidentiality", add
cap label values eyedoctor eyedoctor_lab
label define famcount_lab  .m "No valid response"
label define famcount_lab  .n "Not in universe", add
label define famcount_lab  .l "Logical skip", add
label define famcount_lab  .d "Suppressed for confidentiality", add
cap label values famcount famcount_lab
label define family_r_lab  1  "Two biogical/adoptive parents, currently married"
label define family_r_lab  2  "Two biogical/adoptive parents, not currently married", add
label define family_r_lab  3  "Two parents (at least one not biological/adoptive), currently married", add
label define family_r_lab  4  "Two parents (at least one not biological/adoptive), not currently married", add
label define family_r_lab  5  "Single mother", add
label define family_r_lab  6  "Single father", add
label define family_r_lab  7  "Grandparent household", add
label define family_r_lab  8  "Other relation", add
label define family_r_lab  .m "No valid response", add
label define family_r_lab  .n "Not in universe", add
label define family_r_lab  .l "Logical skip", add
label define family_r_lab  .d "Suppressed for confidentiality", add
cap label values family_r family_r_lab
label define fasd_lab  1  "Yes"
label define fasd_lab  2  "No", add
label define fasd_lab  .m "No valid response", add
label define fasd_lab  .n "Not in universe", add
label define fasd_lab  .l "Logical skip", add
label define fasd_lab  .d "Suppressed for confidentiality", add
cap label values fasd fasd_lab
label define fipsst_lab  .m "No valid response"
label define fipsst_lab  .n "Not in universe", add
label define fipsst_lab  .l "Logical skip", add
label define fipsst_lab  .d "Suppressed for confidentiality", add
cap label values fipsst fipsst_lab
label define focuson_lab  1  "Always"
label define focuson_lab  2  "Most of the time", add
label define focuson_lab  3  "About half the time", add
label define focuson_lab  4  "Sometimes", add
label define focuson_lab  5  "Never", add
label define focuson_lab  .m "No valid response", add
label define focuson_lab  .n "Not in universe", add
label define focuson_lab  .l "Logical skip", add
label define focuson_lab  .d "Suppressed for confidentiality", add
cap label values focuson focuson_lab
label define foodsit_lab  1  "We could always afford to eat good nutritious meals."
label define foodsit_lab  2  "We could always afford enough to eat but not always the kinds of food we should eat.", add
label define foodsit_lab  3  "Sometimes we could not afford enough to eat.", add
label define foodsit_lab  4  "Often we could not afford enough to eat.", add
label define foodsit_lab  .m "No valid response", add
label define foodsit_lab  .n "Not in universe", add
label define foodsit_lab  .l "Logical skip", add
label define foodsit_lab  .d "Suppressed for confidentiality", add
cap label values foodsit foodsit_lab
label define formtype_lab  .m "No valid response"
label define formtype_lab  .n "Not in universe", add
label define formtype_lab  .l "Logical skip", add
label define formtype_lab  .d "Suppressed for confidentiality", add
cap label values formtype formtype_lab
label define fpl_i1_lab  .m "No valid response"
label define fpl_i1_lab  .n "Not in universe", add
label define fpl_i1_lab  .l "Logical skip", add
label define fpl_i1_lab  .d "Suppressed for confidentiality", add
cap label values fpl_i1 fpl_i1_lab
label define fpl_i2_lab  .m "No valid response"
label define fpl_i2_lab  .n "Not in universe", add
label define fpl_i2_lab  .l "Logical skip", add
label define fpl_i2_lab  .d "Suppressed for confidentiality", add
cap label values fpl_i2 fpl_i2_lab
label define fpl_i3_lab  .m "No valid response"
label define fpl_i3_lab  .n "Not in universe", add
label define fpl_i3_lab  .l "Logical skip", add
label define fpl_i3_lab  .d "Suppressed for confidentiality", add
cap label values fpl_i3 fpl_i3_lab
label define fpl_i4_lab  .m "No valid response"
label define fpl_i4_lab  .n "Not in universe", add
label define fpl_i4_lab  .l "Logical skip", add
label define fpl_i4_lab  .d "Suppressed for confidentiality", add
cap label values fpl_i4 fpl_i4_lab
label define fpl_i5_lab  .m "No valid response"
label define fpl_i5_lab  .n "Not in universe", add
label define fpl_i5_lab  .l "Logical skip", add
label define fpl_i5_lab  .d "Suppressed for confidentiality", add
cap label values fpl_i5 fpl_i5_lab
label define fpl_i6_lab  .m "No valid response"
label define fpl_i6_lab  .n "Not in universe", add
label define fpl_i6_lab  .l "Logical skip", add
label define fpl_i6_lab  .d "Suppressed for confidentiality", add
cap label values fpl_i6 fpl_i6_lab
label define fpl_if_lab  1  "Imputed"
label define fpl_if_lab  0  "Not imputed", add
label define fpl_if_lab  .m "No valid response", add
label define fpl_if_lab  .n "Not in universe", add
label define fpl_if_lab  .l "Logical skip", add
label define fpl_if_lab  .d "Suppressed for confidentiality", add
cap label values fpl_if fpl_if_lab
label define frstformula_day_s_lab  .m "No valid response"
label define frstformula_day_s_lab  .n "Not in universe", add
label define frstformula_day_s_lab  .l "Logical skip", add
label define frstformula_day_s_lab  .d "Suppressed for confidentiality", add
cap label values frstformula_day_s frstformula_day_s_lab
label define frstformula_mo_s_lab  .m "No valid response"
label define frstformula_mo_s_lab  .n "Not in universe", add
label define frstformula_mo_s_lab  .l "Logical skip", add
label define frstformula_mo_s_lab  .d "Suppressed for confidentiality", add
cap label values frstformula_mo_s frstformula_mo_s_lab
label define frstformula_wk_s_lab  .m "No valid response"
label define frstformula_wk_s_lab  .n "Not in universe", add
label define frstformula_wk_s_lab  .l "Logical skip", add
label define frstformula_wk_s_lab  .d "Suppressed for confidentiality", add
cap label values frstformula_wk_s frstformula_wk_s_lab
label define frstsolids_day_s_lab  .m "No valid response"
label define frstsolids_day_s_lab  .n "Not in universe", add
label define frstsolids_day_s_lab  .l "Logical skip", add
label define frstsolids_day_s_lab  .d "Suppressed for confidentiality", add
cap label values frstsolids_day_s frstsolids_day_s_lab
label define frstsolids_mo_s_lab  .m "No valid response"
label define frstsolids_mo_s_lab  .n "Not in universe", add
label define frstsolids_mo_s_lab  .l "Logical skip", add
label define frstsolids_mo_s_lab  .d "Suppressed for confidentiality", add
cap label values frstsolids_mo_s frstsolids_mo_s_lab
label define frstsolids_wk_s_lab  .m "No valid response"
label define frstsolids_wk_s_lab  .n "Not in universe", add
label define frstsolids_wk_s_lab  .l "Logical skip", add
label define frstsolids_wk_s_lab  .d "Suppressed for confidentiality", add
cap label values frstsolids_wk_s frstsolids_wk_s_lab
label define fruit_lab  1  "This child did not eat fruit"
label define fruit_lab  2  "1-3 times during the past week", add
label define fruit_lab  3  "4-6 times during the past week", add
label define fruit_lab  4  "1 time per day", add
label define fruit_lab  5  "2 times per day", add
label define fruit_lab  6  "3 or more times per day", add
label define fruit_lab  .m "No valid response", add
label define fruit_lab  .n "Not in universe", add
label define fruit_lab  .l "Logical skip", add
label define fruit_lab  .d "Suppressed for confidentiality", add
cap label values fruit fruit_lab
label define fwc_lab  .m "No valid response"
label define fwc_lab  .n "Not in universe", add
label define fwc_lab  .l "Logical skip", add
label define fwc_lab  .d "Suppressed for confidentiality", add
cap label values fwc fwc_lab
label define gainskills_lab  1  "Yes"
label define gainskills_lab  2  "No", add
label define gainskills_lab  3  "Don't Know", add
label define gainskills_lab  .m "No valid response", add
label define gainskills_lab  .n "Not in universe", add
label define gainskills_lab  .l "Logical skip", add
label define gainskills_lab  .d "Suppressed for confidentiality", add
cap label values gainskills gainskills_lab
label define genetic_lab  1  "Yes"
label define genetic_lab  2  "No", add
label define genetic_lab  .m "No valid response", add
label define genetic_lab  .n "Not in universe", add
label define genetic_lab  .l "Logical skip", add
label define genetic_lab  .d "Suppressed for confidentiality", add
cap label values genetic genetic_lab
label define genetic_desc_lab  1  "Mild"
label define genetic_desc_lab  2  "Moderate", add
label define genetic_desc_lab  3  "Severe", add
label define genetic_desc_lab  .m "No valid response", add
label define genetic_desc_lab  .n "Not in universe", add
label define genetic_desc_lab  .l "Logical skip", add
label define genetic_desc_lab  .d "Suppressed for confidentiality", add
cap label values genetic_desc genetic_desc_lab
label define genetic_screen_lab  1  "Yes"
label define genetic_screen_lab  2  "No", add
label define genetic_screen_lab  .m "No valid response", add
label define genetic_screen_lab  .n "Not in universe", add
label define genetic_screen_lab  .l "Logical skip", add
label define genetic_screen_lab  .d "Suppressed for confidentiality", add
cap label values genetic_screen genetic_screen_lab
label define goforhelp_lab  1  "Definitely agree"
label define goforhelp_lab  2  "Somewhat agree", add
label define goforhelp_lab  3  "Somewhat disagree", add
label define goforhelp_lab  4  "Definitely disagree", add
label define goforhelp_lab  .m "No valid response", add
label define goforhelp_lab  .n "Not in universe", add
label define goforhelp_lab  .l "Logical skip", add
label define goforhelp_lab  .d "Suppressed for confidentiality", add
cap label values goforhelp goforhelp_lab
label define grades_lab  1  "Mostly A's"
label define grades_lab  2  "Mostly A's and B's", add
label define grades_lab  3  "Mostly B's and C's", add
label define grades_lab  4  "Mostly C's and D's", add
label define grades_lab  5  "Mostly D's or lower", add
label define grades_lab  6  "This child's school does not give these grades", add
label define grades_lab  .m "No valid response", add
label define grades_lab  .n "Not in universe", add
label define grades_lab  .l "Logical skip", add
label define grades_lab  .d "Suppressed for confidentiality", add
cap label values grades grades_lab
label define groupofobjects_lab  1  "Always"
label define groupofobjects_lab  2  "Most of the time", add
label define groupofobjects_lab  3  "About half the time", add
label define groupofobjects_lab  4  "Sometimes", add
label define groupofobjects_lab  5  "Never", add
label define groupofobjects_lab  .m "No valid response", add
label define groupofobjects_lab  .n "Not in universe", add
label define groupofobjects_lab  .l "Logical skip", add
label define groupofobjects_lab  .d "Suppressed for confidentiality", add
cap label values groupofobjects groupofobjects_lab
label define gumbleed_lab  1  "Yes"
label define gumbleed_lab  2  "No", add
label define gumbleed_lab  .m "No valid response", add
label define gumbleed_lab  .n "Not in universe", add
label define gumbleed_lab  .l "Logical skip", add
label define gumbleed_lab  .d "Suppressed for confidentiality", add
cap label values gumbleed gumbleed_lab
label define hands_lab  1  "Yes"
label define hands_lab  2  "No", add
label define hands_lab  .m "No valid response", add
label define hands_lab  .n "Not in universe", add
label define hands_lab  .l "Logical skip", add
label define hands_lab  .d "Suppressed for confidentiality", add
cap label values hands hands_lab
label define hardwork_lab  1  "Always"
label define hardwork_lab  2  "Most of the time", add
label define hardwork_lab  3  "About half the time", add
label define hardwork_lab  4  "Sometimes", add
label define hardwork_lab  5  "Never", add
label define hardwork_lab  .m "No valid response", add
label define hardwork_lab  .n "Not in universe", add
label define hardwork_lab  .l "Logical skip", add
label define hardwork_lab  .d "Suppressed for confidentiality", add
cap label values hardwork hardwork_lab
label define hcability_lab  1  "This child does not have any health conditions"
label define hcability_lab  2  "Never", add
label define hcability_lab  3  "Sometimes", add
label define hcability_lab  4  "Usually", add
label define hcability_lab  5  "Always", add
label define hcability_lab  .m "No valid response", add
label define hcability_lab  .n "Not in universe", add
label define hcability_lab  .l "Logical skip", add
label define hcability_lab  .d "Suppressed for confidentiality", add
cap label values hcability hcability_lab
label define hccovoth_lab  1  "Yes"
label define hccovoth_lab  2  "No", add
label define hccovoth_lab  .m "No valid response", add
label define hccovoth_lab  .n "Not in universe", add
label define hccovoth_lab  .l "Logical skip", add
label define hccovoth_lab  .d "Suppressed for confidentiality", add
cap label values hccovoth hccovoth_lab
label define hcextent_lab  1  "Very little"
label define hcextent_lab  2  "Somewhat", add
label define hcextent_lab  3  "A great deal", add
label define hcextent_lab  .m "No valid response", add
label define hcextent_lab  .n "Not in universe", add
label define hcextent_lab  .l "Logical skip", add
label define hcextent_lab  .d "Suppressed for confidentiality", add
cap label values hcextent hcextent_lab
label define headache_lab  1  "Yes"
label define headache_lab  2  "No", add
label define headache_lab  .m "No valid response", add
label define headache_lab  .n "Not in universe", add
label define headache_lab  .l "Logical skip", add
label define headache_lab  .d "Suppressed for confidentiality", add
cap label values headache headache_lab
label define headache_curr_lab  1  "Yes"
label define headache_curr_lab  2  "No", add
label define headache_curr_lab  .m "No valid response", add
label define headache_curr_lab  .n "Not in universe", add
label define headache_curr_lab  .l "Logical skip", add
label define headache_curr_lab  .d "Suppressed for confidentiality", add
cap label values headache_curr headache_curr_lab
label define headache_desc_lab  1  "Mild"
label define headache_desc_lab  2  "Moderate", add
label define headache_desc_lab  3  "Severe", add
label define headache_desc_lab  .m "No valid response", add
label define headache_desc_lab  .n "Not in universe", add
label define headache_desc_lab  .l "Logical skip", add
label define headache_desc_lab  .d "Suppressed for confidentiality", add
cap label values headache_desc headache_desc_lab
label define healthknow_lab  1  "Yes"
label define healthknow_lab  2  "No", add
label define healthknow_lab  .m "No valid response", add
label define healthknow_lab  .n "Not in universe", add
label define healthknow_lab  .l "Logical skip", add
label define healthknow_lab  .d "Suppressed for confidentiality", add
cap label values healthknow healthknow_lab
label define heart_lab  1  "Yes"
label define heart_lab  2  "No", add
label define heart_lab  .m "No valid response", add
label define heart_lab  .n "Not in universe", add
label define heart_lab  .l "Logical skip", add
label define heart_lab  .d "Suppressed for confidentiality", add
cap label values heart heart_lab
label define heart_born_lab  1  "Yes"
label define heart_born_lab  2  "No", add
label define heart_born_lab  .m "No valid response", add
label define heart_born_lab  .n "Not in universe", add
label define heart_born_lab  .l "Logical skip", add
label define heart_born_lab  .d "Suppressed for confidentiality", add
cap label values heart_born heart_born_lab
label define heart_curr_lab  1  "Yes"
label define heart_curr_lab  2  "No", add
label define heart_curr_lab  .m "No valid response", add
label define heart_curr_lab  .n "Not in universe", add
label define heart_curr_lab  .l "Logical skip", add
label define heart_curr_lab  .d "Suppressed for confidentiality", add
cap label values heart_curr heart_curr_lab
label define heart_desc_lab  1  "Mild"
label define heart_desc_lab  2  "Moderate", add
label define heart_desc_lab  3  "Severe", add
label define heart_desc_lab  .m "No valid response", add
label define heart_desc_lab  .n "Not in universe", add
label define heart_desc_lab  .l "Logical skip", add
label define heart_desc_lab  .d "Suppressed for confidentiality", add
cap label values heart_desc heart_desc_lab
label define hemophilia_lab  1  "Yes"
label define hemophilia_lab  2  "No", add
label define hemophilia_lab  .m "No valid response", add
label define hemophilia_lab  .n "Not in universe", add
label define hemophilia_lab  .l "Logical skip", add
label define hemophilia_lab  .d "Suppressed for confidentiality", add
cap label values hemophilia hemophilia_lab
label define hhcount_lab  .m "No valid response"
label define hhcount_lab  .n "Not in universe", add
label define hhcount_lab  .l "Logical skip", add
label define hhcount_lab  .d "Suppressed for confidentiality", add
cap label values hhcount hhcount_lab
label define hhcount_if_lab  1  "Imputed"
label define hhcount_if_lab  0  "Not imputed", add
label define hhcount_if_lab  .m "No valid response", add
label define hhcount_if_lab  .n "Not in universe", add
label define hhcount_if_lab  .l "Logical skip", add
label define hhcount_if_lab  .d "Suppressed for confidentiality", add
cap label values hhcount_if hhcount_if_lab
label define hhid_lab  .m "No valid response"
label define hhid_lab  .n "Not in universe", add
label define hhid_lab  .l "Logical skip", add
label define hhid_lab  .d "Suppressed for confidentiality", add
cap label values hhid hhid_lab
label define hhlanguage_lab  1  "English"
label define hhlanguage_lab  2  "Spanish", add
label define hhlanguage_lab  3  "Other", add
label define hhlanguage_lab  .m "No valid response", add
label define hhlanguage_lab  .n "Not in universe", add
label define hhlanguage_lab  .l "Logical skip", add
label define hhlanguage_lab  .d "Suppressed for confidentiality", add
cap label values hhlanguage hhlanguage_lab
label define higrade_lab  1  "Less than high school"
label define higrade_lab  2  "High school (including vocational, trade, or business school)", add
label define higrade_lab  3  "More than high school", add
label define higrade_lab  .m "No valid response", add
label define higrade_lab  .n "Not in universe", add
label define higrade_lab  .l "Logical skip", add
label define higrade_lab  .d "Suppressed for confidentiality", add
cap label values higrade higrade_lab
label define higrade_tvis_lab  1  "Less than high school"
label define higrade_tvis_lab  2  "High school (including vocational, trade, or business school)", add
label define higrade_tvis_lab  3  "Some college or Associate Degree", add
label define higrade_tvis_lab  4  "College degree or higher", add
label define higrade_tvis_lab  .m "No valid response", add
label define higrade_tvis_lab  .n "Not in universe", add
label define higrade_tvis_lab  .l "Logical skip", add
label define higrade_tvis_lab  .d "Suppressed for confidentiality", add
cap label values higrade_tvis higrade_tvis_lab
label define homeevic_lab  1  "Always"
label define homeevic_lab  2  "Usually", add
label define homeevic_lab  3  "Sometimes", add
label define homeevic_lab  4  "Rarely", add
label define homeevic_lab  5  "Never", add
label define homeevic_lab  .m "No valid response", add
label define homeevic_lab  .n "Not in universe", add
label define homeevic_lab  .l "Logical skip", add
label define homeevic_lab  .d "Suppressed for confidentiality", add
cap label values homeevic homeevic_lab
label define hopeful_lab  1  "All of the time"
label define hopeful_lab  2  "Most of the time", add
label define hopeful_lab  3  "Some of the time", add
label define hopeful_lab  4  "None of the time", add
label define hopeful_lab  .m "No valid response", add
label define hopeful_lab  .n "Not in universe", add
label define hopeful_lab  .l "Logical skip", add
label define hopeful_lab  .d "Suppressed for confidentiality", add
cap label values hopeful hopeful_lab
label define hospitaler_lab  1  "None"
label define hospitaler_lab  2  "1 time", add
label define hospitaler_lab  3  "2-3 times", add
label define hospitaler_lab  4  "4 or more times", add
label define hospitaler_lab  .m "No valid response", add
label define hospitaler_lab  .n "Not in universe", add
label define hospitaler_lab  .l "Logical skip", add
label define hospitaler_lab  .d "Suppressed for confidentiality", add
cap label values hospitaler hospitaler_lab
label define hospitalstay_lab  1  "Yes"
label define hospitalstay_lab  2  "No", add
label define hospitalstay_lab  .m "No valid response", add
label define hospitalstay_lab  .n "Not in universe", add
label define hospitalstay_lab  .l "Logical skip", add
label define hospitalstay_lab  .d "Suppressed for confidentiality", add
cap label values hospitalstay hospitalstay_lab
label define hoursleep_lab  1  "Less than 6 hours"
label define hoursleep_lab  2  "6 hours", add
label define hoursleep_lab  3  "7 hours", add
label define hoursleep_lab  4  "8 hours", add
label define hoursleep_lab  5  "9 hours", add
label define hoursleep_lab  6  "10 hours", add
label define hoursleep_lab  7  "11 or more hours", add
label define hoursleep_lab  .m "No valid response", add
label define hoursleep_lab  .n "Not in universe", add
label define hoursleep_lab  .l "Logical skip", add
label define hoursleep_lab  .d "Suppressed for confidentiality", add
cap label values hoursleep hoursleep_lab
label define hoursleep05_lab  1  "Less than 7 hours"
label define hoursleep05_lab  2  "7 hours", add
label define hoursleep05_lab  3  "8 hours", add
label define hoursleep05_lab  4  "9 hours", add
label define hoursleep05_lab  5  "10 hours", add
label define hoursleep05_lab  6  "11 hours", add
label define hoursleep05_lab  7  "12 or more hours", add
label define hoursleep05_lab  .m "No valid response", add
label define hoursleep05_lab  .n "Not in universe", add
label define hoursleep05_lab  .l "Logical skip", add
label define hoursleep05_lab  .d "Suppressed for confidentiality", add
cap label values hoursleep05 hoursleep05_lab
label define house_gen_lab  1  "1st generation household [Child is born outside the United States and all reported parents are born outside the United States. At least one parent must be reported as born outside the United States.]"
label define house_gen_lab  2  "2nd generation household [Child is born in the United States and at least one parent is born outside the United States OR child is born outside the United States, one parent is born in the United States and one parent is born outside the United States.]", add
label define house_gen_lab  3  "3rd+ generation [All parents in the household are born in the United States]", add
label define house_gen_lab  4  "Other [Child is born in the United States, parents are not listed.]", add
label define house_gen_lab  .m "No valid response", add
label define house_gen_lab  .n "Not in universe", add
label define house_gen_lab  .l "Logical skip", add
label define house_gen_lab  .d "Suppressed for confidentiality", add
cap label values house_gen house_gen_lab
label define howmuch_lab  1  "$0 (No medical or health-related expenses)"
label define howmuch_lab  2  "$1-$249", add
label define howmuch_lab  3  "$250-$499", add
label define howmuch_lab  4  "$500-$999", add
label define howmuch_lab  5  "$1,000-$5,000", add
label define howmuch_lab  6  "More than $5,000", add
label define howmuch_lab  .m "No valid response", add
label define howmuch_lab  .n "Not in universe", add
label define howmuch_lab  .l "Logical skip", add
label define howmuch_lab  .d "Suppressed for confidentiality", add
cap label values howmuch howmuch_lab
label define hurtsad_lab  1  "Always"
label define hurtsad_lab  2  "Most of the time", add
label define hurtsad_lab  3  "About half the time", add
label define hurtsad_lab  4  "Sometimes", add
label define hurtsad_lab  5  "Never", add
label define hurtsad_lab  .m "No valid response", add
label define hurtsad_lab  .n "Not in universe", add
label define hurtsad_lab  .l "Logical skip", add
label define hurtsad_lab  .d "Suppressed for confidentiality", add
cap label values hurtsad hurtsad_lab
label define inq_edu_lab  1  "High"
label define inq_edu_lab  2  "Low", add
label define inq_edu_lab  .m "No valid response", add
label define inq_edu_lab  .n "Not in universe", add
label define inq_edu_lab  .l "Logical skip", add
label define inq_edu_lab  .d "Suppressed for confidentiality", add
cap label values inq_edu inq_edu_lab
label define inq_employ_lab  1  "High"
label define inq_employ_lab  2  "Low", add
label define inq_employ_lab  .m "No valid response", add
label define inq_employ_lab  .n "Not in universe", add
label define inq_employ_lab  .l "Logical skip", add
label define inq_employ_lab  .d "Suppressed for confidentiality", add
cap label values inq_employ inq_employ_lab
label define inq_home_lab  1  "High"
label define inq_home_lab  2  "Low", add
label define inq_home_lab  .m "No valid response", add
label define inq_home_lab  .n "Not in universe", add
label define inq_home_lab  .l "Logical skip", add
label define inq_home_lab  .d "Suppressed for confidentiality", add
cap label values inq_home inq_home_lab
label define inq_income_lab  1  "High"
label define inq_income_lab  2  "Low", add
label define inq_income_lab  .m "No valid response", add
label define inq_income_lab  .n "Not in universe", add
label define inq_income_lab  .l "Logical skip", add
label define inq_income_lab  .d "Suppressed for confidentiality", add
cap label values inq_income inq_income_lab
label define inq_resseg_lab  1  "High"
label define inq_resseg_lab  2  "Low", add
label define inq_resseg_lab  .m "No valid response", add
label define inq_resseg_lab  .n "Not in universe", add
label define inq_resseg_lab  .l "Logical skip", add
label define inq_resseg_lab  .d "Suppressed for confidentiality", add
cap label values inq_resseg inq_resseg_lab
label define insgap_lab  1  "Insured all 12 months"
label define insgap_lab  2  "Insured during the past 12 months but with gaps in coverage", add
label define insgap_lab  3  "No coverage past 12 months", add
label define insgap_lab  .m "No valid response", add
label define insgap_lab  .n "Not in universe", add
label define insgap_lab  .l "Logical skip", add
label define insgap_lab  .d "Suppressed for confidentiality", add
cap label values insgap insgap_lab
label define instype_lab  1  "Public only (government assistance)"
label define instype_lab  2  "Private only (privately purchased, including through ACA marketplace, through employer, or TRICARE)", add
label define instype_lab  3  "Private and public", add
label define instype_lab  5  "Not insured", add
label define instype_lab  .m "No valid response", add
label define instype_lab  .n "Not in universe", add
label define instype_lab  .l "Logical skip", add
label define instype_lab  .d "Suppressed for confidentiality", add
cap label values instype instype_lab
label define issuecost_lab  1  "Yes"
label define issuecost_lab  2  "No", add
label define issuecost_lab  .m "No valid response", add
label define issuecost_lab  .n "Not in universe", add
label define issuecost_lab  .l "Logical skip", add
label define issuecost_lab  .d "Suppressed for confidentiality", add
cap label values issuecost issuecost_lab
label define k10q11_lab  1  "Yes"
label define k10q11_lab  2  "No", add
label define k10q11_lab  .m "No valid response", add
label define k10q11_lab  .n "Not in universe", add
label define k10q11_lab  .l "Logical skip", add
label define k10q11_lab  .d "Suppressed for confidentiality", add
cap label values k10q11 k10q11_lab
label define k10q12_lab  1  "Yes"
label define k10q12_lab  2  "No", add
label define k10q12_lab  .m "No valid response", add
label define k10q12_lab  .n "Not in universe", add
label define k10q12_lab  .l "Logical skip", add
label define k10q12_lab  .d "Suppressed for confidentiality", add
cap label values k10q12 k10q12_lab
label define k10q13_lab  1  "Yes"
label define k10q13_lab  2  "No", add
label define k10q13_lab  .m "No valid response", add
label define k10q13_lab  .n "Not in universe", add
label define k10q13_lab  .l "Logical skip", add
label define k10q13_lab  .d "Suppressed for confidentiality", add
cap label values k10q13 k10q13_lab
label define k10q14_lab  1  "Yes"
label define k10q14_lab  2  "No", add
label define k10q14_lab  .m "No valid response", add
label define k10q14_lab  .n "Not in universe", add
label define k10q14_lab  .l "Logical skip", add
label define k10q14_lab  .d "Suppressed for confidentiality", add
cap label values k10q14 k10q14_lab
label define k10q20_lab  1  "Yes"
label define k10q20_lab  2  "No", add
label define k10q20_lab  .m "No valid response", add
label define k10q20_lab  .n "Not in universe", add
label define k10q20_lab  .l "Logical skip", add
label define k10q20_lab  .d "Suppressed for confidentiality", add
cap label values k10q20 k10q20_lab
label define k10q22_lab  1  "Yes"
label define k10q22_lab  2  "No", add
label define k10q22_lab  .m "No valid response", add
label define k10q22_lab  .n "Not in universe", add
label define k10q22_lab  .l "Logical skip", add
label define k10q22_lab  .d "Suppressed for confidentiality", add
cap label values k10q22 k10q22_lab
label define k10q23_lab  1  "Yes"
label define k10q23_lab  2  "No", add
label define k10q23_lab  .m "No valid response", add
label define k10q23_lab  .n "Not in universe", add
label define k10q23_lab  .l "Logical skip", add
label define k10q23_lab  .d "Suppressed for confidentiality", add
cap label values k10q23 k10q23_lab
label define k10q30_lab  1  "Definitely agree"
label define k10q30_lab  2  "Somewhat agree", add
label define k10q30_lab  3  "Somewhat disagree", add
label define k10q30_lab  4  "Definitely disagree", add
label define k10q30_lab  .m "No valid response", add
label define k10q30_lab  .n "Not in universe", add
label define k10q30_lab  .l "Logical skip", add
label define k10q30_lab  .d "Suppressed for confidentiality", add
cap label values k10q30 k10q30_lab
label define k10q31_lab  1  "Definitely agree"
label define k10q31_lab  2  "Somewhat agree", add
label define k10q31_lab  3  "Somewhat disagree", add
label define k10q31_lab  4  "Definitely disagree", add
label define k10q31_lab  .m "No valid response", add
label define k10q31_lab  .n "Not in universe", add
label define k10q31_lab  .l "Logical skip", add
label define k10q31_lab  .d "Suppressed for confidentiality", add
cap label values k10q31 k10q31_lab
label define k10q40_r_lab  1  "Definitely agree"
label define k10q40_r_lab  2  "Somewhat agree", add
label define k10q40_r_lab  3  "Somewhat disagree", add
label define k10q40_r_lab  4  "Definitely disagree", add
label define k10q40_r_lab  .m "No valid response", add
label define k10q40_r_lab  .n "Not in universe", add
label define k10q40_r_lab  .l "Logical skip", add
label define k10q40_r_lab  .d "Suppressed for confidentiality", add
cap label values k10q40_r k10q40_r_lab
label define k10q41_r_lab  1  "Definitely agree"
label define k10q41_r_lab  2  "Somewhat agree", add
label define k10q41_r_lab  3  "Somewhat disagree", add
label define k10q41_r_lab  4  "Definitely disagree", add
label define k10q41_r_lab  .m "No valid response", add
label define k10q41_r_lab  .n "Not in universe", add
label define k10q41_r_lab  .l "Logical skip", add
label define k10q41_r_lab  .d "Suppressed for confidentiality", add
cap label values k10q41_r k10q41_r_lab
label define k11q03r_lab  1  "Yes"
label define k11q03r_lab  2  "No", add
label define k11q03r_lab  .m "No valid response", add
label define k11q03r_lab  .n "Not in universe", add
label define k11q03r_lab  .l "Logical skip", add
label define k11q03r_lab  .d "Suppressed for confidentiality", add
cap label values k11q03r k11q03r_lab
label define k11q43r_lab  .m "No valid response"
label define k11q43r_lab  .n "Not in universe", add
label define k11q43r_lab  .l "Logical skip", add
label define k11q43r_lab  .d "Suppressed for confidentiality", add
cap label values k11q43r k11q43r_lab
label define k11q60_lab  1  "Yes"
label define k11q60_lab  2  "No", add
label define k11q60_lab  .m "No valid response", add
label define k11q60_lab  .n "Not in universe", add
label define k11q60_lab  .l "Logical skip", add
label define k11q60_lab  .d "Suppressed for confidentiality", add
cap label values k11q60 k11q60_lab
label define k11q61_lab  1  "Yes"
label define k11q61_lab  2  "No", add
label define k11q61_lab  .m "No valid response", add
label define k11q61_lab  .n "Not in universe", add
label define k11q61_lab  .l "Logical skip", add
label define k11q61_lab  .d "Suppressed for confidentiality", add
cap label values k11q61 k11q61_lab
label define k11q62_lab  1  "Yes"
label define k11q62_lab  2  "No", add
label define k11q62_lab  .m "No valid response", add
label define k11q62_lab  .n "Not in universe", add
label define k11q62_lab  .l "Logical skip", add
label define k11q62_lab  .d "Suppressed for confidentiality", add
cap label values k11q62 k11q62_lab
label define k12q01_a_lab  1  "Yes"
label define k12q01_a_lab  2  "No", add
label define k12q01_a_lab  .m "No valid response", add
label define k12q01_a_lab  .n "Not in universe", add
label define k12q01_a_lab  .l "Logical skip", add
label define k12q01_a_lab  .d "Suppressed for confidentiality", add
cap label values k12q01_a k12q01_a_lab
label define k12q01_b_lab  1  "Yes"
label define k12q01_b_lab  2  "No", add
label define k12q01_b_lab  .m "No valid response", add
label define k12q01_b_lab  .n "Not in universe", add
label define k12q01_b_lab  .l "Logical skip", add
label define k12q01_b_lab  .d "Suppressed for confidentiality", add
cap label values k12q01_b k12q01_b_lab
label define k12q01_c_lab  1  "Yes"
label define k12q01_c_lab  2  "No", add
label define k12q01_c_lab  .m "No valid response", add
label define k12q01_c_lab  .n "Not in universe", add
label define k12q01_c_lab  .l "Logical skip", add
label define k12q01_c_lab  .d "Suppressed for confidentiality", add
cap label values k12q01_c k12q01_c_lab
label define k12q01_d_lab  1  "Yes"
label define k12q01_d_lab  2  "No", add
label define k12q01_d_lab  .m "No valid response", add
label define k12q01_d_lab  .n "Not in universe", add
label define k12q01_d_lab  .l "Logical skip", add
label define k12q01_d_lab  .d "Suppressed for confidentiality", add
cap label values k12q01_d k12q01_d_lab
label define k12q01_e_lab  1  "Yes"
label define k12q01_e_lab  2  "No", add
label define k12q01_e_lab  .m "No valid response", add
label define k12q01_e_lab  .n "Not in universe", add
label define k12q01_e_lab  .l "Logical skip", add
label define k12q01_e_lab  .d "Suppressed for confidentiality", add
cap label values k12q01_e k12q01_e_lab
label define k12q01_f_lab  1  "Yes"
label define k12q01_f_lab  2  "No", add
label define k12q01_f_lab  .m "No valid response", add
label define k12q01_f_lab  .n "Not in universe", add
label define k12q01_f_lab  .l "Logical skip", add
label define k12q01_f_lab  .d "Suppressed for confidentiality", add
cap label values k12q01_f k12q01_f_lab
label define k12q01_g_lab  1  "Yes"
label define k12q01_g_lab  2  "No", add
label define k12q01_g_lab  .m "No valid response", add
label define k12q01_g_lab  .n "Not in universe", add
label define k12q01_g_lab  .l "Logical skip", add
label define k12q01_g_lab  .d "Suppressed for confidentiality", add
cap label values k12q01_g k12q01_g_lab
label define k12q03_lab  1  "Yes"
label define k12q03_lab  2  "No", add
label define k12q03_lab  .m "No valid response", add
label define k12q03_lab  .n "Not in universe", add
label define k12q03_lab  .l "Logical skip", add
label define k12q03_lab  .d "Suppressed for confidentiality", add
cap label values k12q03 k12q03_lab
label define k12q04_lab  1  "Yes"
label define k12q04_lab  2  "No", add
label define k12q04_lab  .m "No valid response", add
label define k12q04_lab  .n "Not in universe", add
label define k12q04_lab  .l "Logical skip", add
label define k12q04_lab  .d "Suppressed for confidentiality", add
cap label values k12q04 k12q04_lab
label define k12q12_lab  1  "Yes"
label define k12q12_lab  2  "No", add
label define k12q12_lab  .m "No valid response", add
label define k12q12_lab  .n "Not in universe", add
label define k12q12_lab  .l "Logical skip", add
label define k12q12_lab  .d "Suppressed for confidentiality", add
cap label values k12q12 k12q12_lab
label define k2q01_lab  1  "Excellent"
label define k2q01_lab  2  "Very Good", add
label define k2q01_lab  3  "Good", add
label define k2q01_lab  4  "Fair", add
label define k2q01_lab  5  "Poor", add
label define k2q01_lab  .m "No valid response", add
label define k2q01_lab  .n "Not in universe", add
label define k2q01_lab  .l "Logical skip", add
label define k2q01_lab  .d "Suppressed for confidentiality", add
cap label values k2q01 k2q01_lab
label define k2q01_d_lab  6  "This child does not have any teeth [T1 only]"
label define k2q01_d_lab  1  "Excellent", add
label define k2q01_d_lab  2  "Very Good", add
label define k2q01_d_lab  3  "Good", add
label define k2q01_d_lab  4  "Fair", add
label define k2q01_d_lab  5  "Poor", add
label define k2q01_d_lab  .m "No valid response", add
label define k2q01_d_lab  .n "Not in universe", add
label define k2q01_d_lab  .l "Logical skip", add
label define k2q01_d_lab  .d "Suppressed for confidentiality", add
cap label values k2q01_d k2q01_d_lab
label define k2q05_lab  1  "Yes"
label define k2q05_lab  2  "No", add
label define k2q05_lab  .m "No valid response", add
label define k2q05_lab  .n "Not in universe", add
label define k2q05_lab  .l "Logical skip", add
label define k2q05_lab  .d "Suppressed for confidentiality", add
cap label values k2q05 k2q05_lab
label define k2q30a_lab  1  "Yes"
label define k2q30a_lab  2  "No", add
label define k2q30a_lab  .m "No valid response", add
label define k2q30a_lab  .n "Not in universe", add
label define k2q30a_lab  .l "Logical skip", add
label define k2q30a_lab  .d "Suppressed for confidentiality", add
cap label values k2q30a k2q30a_lab
label define k2q30b_lab  1  "Yes"
label define k2q30b_lab  2  "No", add
label define k2q30b_lab  .m "No valid response", add
label define k2q30b_lab  .n "Not in universe", add
label define k2q30b_lab  .l "Logical skip", add
label define k2q30b_lab  .d "Suppressed for confidentiality", add
cap label values k2q30b k2q30b_lab
label define k2q30c_lab  1  "Mild"
label define k2q30c_lab  2  "Moderate", add
label define k2q30c_lab  3  "Severe", add
label define k2q30c_lab  .m "No valid response", add
label define k2q30c_lab  .n "Not in universe", add
label define k2q30c_lab  .l "Logical skip", add
label define k2q30c_lab  .d "Suppressed for confidentiality", add
cap label values k2q30c k2q30c_lab
label define k2q31a_lab  1  "Yes"
label define k2q31a_lab  2  "No", add
label define k2q31a_lab  .m "No valid response", add
label define k2q31a_lab  .n "Not in universe", add
label define k2q31a_lab  .l "Logical skip", add
label define k2q31a_lab  .d "Suppressed for confidentiality", add
cap label values k2q31a k2q31a_lab
label define k2q31b_lab  1  "Yes"
label define k2q31b_lab  2  "No", add
label define k2q31b_lab  .m "No valid response", add
label define k2q31b_lab  .n "Not in universe", add
label define k2q31b_lab  .l "Logical skip", add
label define k2q31b_lab  .d "Suppressed for confidentiality", add
cap label values k2q31b k2q31b_lab
label define k2q31c_lab  1  "Mild"
label define k2q31c_lab  2  "Moderate", add
label define k2q31c_lab  3  "Severe", add
label define k2q31c_lab  .m "No valid response", add
label define k2q31c_lab  .n "Not in universe", add
label define k2q31c_lab  .l "Logical skip", add
label define k2q31c_lab  .d "Suppressed for confidentiality", add
cap label values k2q31c k2q31c_lab
label define k2q31d_lab  1  "Yes"
label define k2q31d_lab  2  "No", add
label define k2q31d_lab  .m "No valid response", add
label define k2q31d_lab  .n "Not in universe", add
label define k2q31d_lab  .l "Logical skip", add
label define k2q31d_lab  .d "Suppressed for confidentiality", add
cap label values k2q31d k2q31d_lab
label define k2q32a_lab  1  "Yes"
label define k2q32a_lab  2  "No", add
label define k2q32a_lab  .m "No valid response", add
label define k2q32a_lab  .n "Not in universe", add
label define k2q32a_lab  .l "Logical skip", add
label define k2q32a_lab  .d "Suppressed for confidentiality", add
cap label values k2q32a k2q32a_lab
label define k2q32b_lab  1  "Yes"
label define k2q32b_lab  2  "No", add
label define k2q32b_lab  .m "No valid response", add
label define k2q32b_lab  .n "Not in universe", add
label define k2q32b_lab  .l "Logical skip", add
label define k2q32b_lab  .d "Suppressed for confidentiality", add
cap label values k2q32b k2q32b_lab
label define k2q32c_lab  1  "Mild"
label define k2q32c_lab  2  "Moderate", add
label define k2q32c_lab  3  "Severe", add
label define k2q32c_lab  .m "No valid response", add
label define k2q32c_lab  .n "Not in universe", add
label define k2q32c_lab  .l "Logical skip", add
label define k2q32c_lab  .d "Suppressed for confidentiality", add
cap label values k2q32c k2q32c_lab
label define k2q33a_lab  1  "Yes"
label define k2q33a_lab  2  "No", add
label define k2q33a_lab  .m "No valid response", add
label define k2q33a_lab  .n "Not in universe", add
label define k2q33a_lab  .l "Logical skip", add
label define k2q33a_lab  .d "Suppressed for confidentiality", add
cap label values k2q33a k2q33a_lab
label define k2q33b_lab  1  "Yes"
label define k2q33b_lab  2  "No", add
label define k2q33b_lab  .m "No valid response", add
label define k2q33b_lab  .n "Not in universe", add
label define k2q33b_lab  .l "Logical skip", add
label define k2q33b_lab  .d "Suppressed for confidentiality", add
cap label values k2q33b k2q33b_lab
label define k2q33c_lab  1  "Mild"
label define k2q33c_lab  2  "Moderate", add
label define k2q33c_lab  3  "Severe", add
label define k2q33c_lab  .m "No valid response", add
label define k2q33c_lab  .n "Not in universe", add
label define k2q33c_lab  .l "Logical skip", add
label define k2q33c_lab  .d "Suppressed for confidentiality", add
cap label values k2q33c k2q33c_lab
label define k2q34a_lab  1  "Yes"
label define k2q34a_lab  2  "No", add
label define k2q34a_lab  .m "No valid response", add
label define k2q34a_lab  .n "Not in universe", add
label define k2q34a_lab  .l "Logical skip", add
label define k2q34a_lab  .d "Suppressed for confidentiality", add
cap label values k2q34a k2q34a_lab
label define k2q34b_lab  1  "Yes"
label define k2q34b_lab  2  "No", add
label define k2q34b_lab  .m "No valid response", add
label define k2q34b_lab  .n "Not in universe", add
label define k2q34b_lab  .l "Logical skip", add
label define k2q34b_lab  .d "Suppressed for confidentiality", add
cap label values k2q34b k2q34b_lab
label define k2q34c_lab  1  "Mild"
label define k2q34c_lab  2  "Moderate", add
label define k2q34c_lab  3  "Severe", add
label define k2q34c_lab  .m "No valid response", add
label define k2q34c_lab  .n "Not in universe", add
label define k2q34c_lab  .l "Logical skip", add
label define k2q34c_lab  .d "Suppressed for confidentiality", add
cap label values k2q34c k2q34c_lab
label define k2q35a_lab  1  "Yes"
label define k2q35a_lab  2  "No", add
label define k2q35a_lab  .m "No valid response", add
label define k2q35a_lab  .n "Not in universe", add
label define k2q35a_lab  .l "Logical skip", add
label define k2q35a_lab  .d "Suppressed for confidentiality", add
cap label values k2q35a k2q35a_lab
label define k2q35a_1_years_lab  .m "No valid response"
label define k2q35a_1_years_lab  .n "Not in universe", add
label define k2q35a_1_years_lab  .l "Logical skip", add
label define k2q35a_1_years_lab  .d "Suppressed for confidentiality", add
cap label values k2q35a_1_years k2q35a_1_years_lab
label define k2q35b_lab  1  "Yes"
label define k2q35b_lab  2  "No", add
label define k2q35b_lab  .m "No valid response", add
label define k2q35b_lab  .n "Not in universe", add
label define k2q35b_lab  .l "Logical skip", add
label define k2q35b_lab  .d "Suppressed for confidentiality", add
cap label values k2q35b k2q35b_lab
label define k2q35c_lab  1  "Mild"
label define k2q35c_lab  2  "Moderate", add
label define k2q35c_lab  3  "Severe", add
label define k2q35c_lab  .m "No valid response", add
label define k2q35c_lab  .n "Not in universe", add
label define k2q35c_lab  .l "Logical skip", add
label define k2q35c_lab  .d "Suppressed for confidentiality", add
cap label values k2q35c k2q35c_lab
label define k2q35d_lab  1  "Primary Care Provider"
label define k2q35d_lab  2  "Specialist", add
label define k2q35d_lab  3  "School Psychologist/Counselor", add
label define k2q35d_lab  4  "Other Psychologist (Non-School)", add
label define k2q35d_lab  5  "Psychiatrist", add
label define k2q35d_lab  6  "Other", add
label define k2q35d_lab  7  "Don't Know", add
label define k2q35d_lab  .m "No valid response", add
label define k2q35d_lab  .n "Not in universe", add
label define k2q35d_lab  .l "Logical skip", add
label define k2q35d_lab  .d "Suppressed for confidentiality", add
cap label values k2q35d k2q35d_lab
label define k2q36a_lab  1  "Yes"
label define k2q36a_lab  2  "No", add
label define k2q36a_lab  .m "No valid response", add
label define k2q36a_lab  .n "Not in universe", add
label define k2q36a_lab  .l "Logical skip", add
label define k2q36a_lab  .d "Suppressed for confidentiality", add
cap label values k2q36a k2q36a_lab
label define k2q36b_lab  1  "Yes"
label define k2q36b_lab  2  "No", add
label define k2q36b_lab  .m "No valid response", add
label define k2q36b_lab  .n "Not in universe", add
label define k2q36b_lab  .l "Logical skip", add
label define k2q36b_lab  .d "Suppressed for confidentiality", add
cap label values k2q36b k2q36b_lab
label define k2q36c_lab  1  "Mild"
label define k2q36c_lab  2  "Moderate", add
label define k2q36c_lab  3  "Severe", add
label define k2q36c_lab  .m "No valid response", add
label define k2q36c_lab  .n "Not in universe", add
label define k2q36c_lab  .l "Logical skip", add
label define k2q36c_lab  .d "Suppressed for confidentiality", add
cap label values k2q36c k2q36c_lab
label define k2q37a_lab  1  "Yes"
label define k2q37a_lab  2  "No", add
label define k2q37a_lab  .m "No valid response", add
label define k2q37a_lab  .n "Not in universe", add
label define k2q37a_lab  .l "Logical skip", add
label define k2q37a_lab  .d "Suppressed for confidentiality", add
cap label values k2q37a k2q37a_lab
label define k2q37b_lab  1  "Yes"
label define k2q37b_lab  2  "No", add
label define k2q37b_lab  .m "No valid response", add
label define k2q37b_lab  .n "Not in universe", add
label define k2q37b_lab  .l "Logical skip", add
label define k2q37b_lab  .d "Suppressed for confidentiality", add
cap label values k2q37b k2q37b_lab
label define k2q37c_lab  1  "Mild"
label define k2q37c_lab  2  "Moderate", add
label define k2q37c_lab  3  "Severe", add
label define k2q37c_lab  .m "No valid response", add
label define k2q37c_lab  .n "Not in universe", add
label define k2q37c_lab  .l "Logical skip", add
label define k2q37c_lab  .d "Suppressed for confidentiality", add
cap label values k2q37c k2q37c_lab
label define k2q38a_lab  1  "Yes"
label define k2q38a_lab  2  "No", add
label define k2q38a_lab  .m "No valid response", add
label define k2q38a_lab  .n "Not in universe", add
label define k2q38a_lab  .l "Logical skip", add
label define k2q38a_lab  .d "Suppressed for confidentiality", add
cap label values k2q38a k2q38a_lab
label define k2q38b_lab  1  "Yes"
label define k2q38b_lab  2  "No", add
label define k2q38b_lab  .m "No valid response", add
label define k2q38b_lab  .n "Not in universe", add
label define k2q38b_lab  .l "Logical skip", add
label define k2q38b_lab  .d "Suppressed for confidentiality", add
cap label values k2q38b k2q38b_lab
label define k2q38c_lab  1  "Mild"
label define k2q38c_lab  4  "Moderate/Severe", add
label define k2q38c_lab  .m "No valid response", add
label define k2q38c_lab  .n "Not in universe", add
label define k2q38c_lab  .l "Logical skip", add
label define k2q38c_lab  .d "Suppressed for confidentiality", add
cap label values k2q38c k2q38c_lab
label define k2q40a_lab  1  "Yes"
label define k2q40a_lab  2  "No", add
label define k2q40a_lab  .m "No valid response", add
label define k2q40a_lab  .n "Not in universe", add
label define k2q40a_lab  .l "Logical skip", add
label define k2q40a_lab  .d "Suppressed for confidentiality", add
cap label values k2q40a k2q40a_lab
label define k2q40b_lab  1  "Yes"
label define k2q40b_lab  2  "No", add
label define k2q40b_lab  .m "No valid response", add
label define k2q40b_lab  .n "Not in universe", add
label define k2q40b_lab  .l "Logical skip", add
label define k2q40b_lab  .d "Suppressed for confidentiality", add
cap label values k2q40b k2q40b_lab
label define k2q40c_lab  1  "Mild"
label define k2q40c_lab  2  "Moderate", add
label define k2q40c_lab  3  "Severe", add
label define k2q40c_lab  .m "No valid response", add
label define k2q40c_lab  .n "Not in universe", add
label define k2q40c_lab  .l "Logical skip", add
label define k2q40c_lab  .d "Suppressed for confidentiality", add
cap label values k2q40c k2q40c_lab
label define k2q42a_lab  1  "Yes"
label define k2q42a_lab  2  "No", add
label define k2q42a_lab  .m "No valid response", add
label define k2q42a_lab  .n "Not in universe", add
label define k2q42a_lab  .l "Logical skip", add
label define k2q42a_lab  .d "Suppressed for confidentiality", add
cap label values k2q42a k2q42a_lab
label define k2q42b_lab  1  "Yes"
label define k2q42b_lab  2  "No", add
label define k2q42b_lab  .m "No valid response", add
label define k2q42b_lab  .n "Not in universe", add
label define k2q42b_lab  .l "Logical skip", add
label define k2q42b_lab  .d "Suppressed for confidentiality", add
cap label values k2q42b k2q42b_lab
label define k2q42c_lab  1  "Mild"
label define k2q42c_lab  2  "Moderate", add
label define k2q42c_lab  3  "Severe", add
label define k2q42c_lab  .m "No valid response", add
label define k2q42c_lab  .n "Not in universe", add
label define k2q42c_lab  .l "Logical skip", add
label define k2q42c_lab  .d "Suppressed for confidentiality", add
cap label values k2q42c k2q42c_lab
label define k2q43b_lab  1  "Yes"
label define k2q43b_lab  2  "No", add
label define k2q43b_lab  .m "No valid response", add
label define k2q43b_lab  .n "Not in universe", add
label define k2q43b_lab  .l "Logical skip", add
label define k2q43b_lab  .d "Suppressed for confidentiality", add
cap label values k2q43b k2q43b_lab
label define k2q60a_lab  1  "Yes"
label define k2q60a_lab  2  "No", add
label define k2q60a_lab  .m "No valid response", add
label define k2q60a_lab  .n "Not in universe", add
label define k2q60a_lab  .l "Logical skip", add
label define k2q60a_lab  .d "Suppressed for confidentiality", add
cap label values k2q60a k2q60a_lab
label define k2q60b_lab  1  "Yes"
label define k2q60b_lab  2  "No", add
label define k2q60b_lab  .m "No valid response", add
label define k2q60b_lab  .n "Not in universe", add
label define k2q60b_lab  .l "Logical skip", add
label define k2q60b_lab  .d "Suppressed for confidentiality", add
cap label values k2q60b k2q60b_lab
label define k2q60c_lab  1  "Mild"
label define k2q60c_lab  2  "Moderate", add
label define k2q60c_lab  3  "Severe", add
label define k2q60c_lab  .m "No valid response", add
label define k2q60c_lab  .n "Not in universe", add
label define k2q60c_lab  .l "Logical skip", add
label define k2q60c_lab  .d "Suppressed for confidentiality", add
cap label values k2q60c k2q60c_lab
label define k2q61a_lab  1  "Yes"
label define k2q61a_lab  2  "No", add
label define k2q61a_lab  .m "No valid response", add
label define k2q61a_lab  .n "Not in universe", add
label define k2q61a_lab  .l "Logical skip", add
label define k2q61a_lab  .d "Suppressed for confidentiality", add
cap label values k2q61a k2q61a_lab
label define k3q04_r_lab  1  "Yes, this child was covered all 12 months"
label define k3q04_r_lab  2  "Yes, but this child had a gap in coverage", add
label define k3q04_r_lab  3  "No", add
label define k3q04_r_lab  .m "No valid response", add
label define k3q04_r_lab  .n "Not in universe", add
label define k3q04_r_lab  .l "Logical skip", add
label define k3q04_r_lab  .d "Suppressed for confidentiality", add
cap label values k3q04_r k3q04_r_lab
label define k3q20_lab  1  "Always"
label define k3q20_lab  2  "Usually", add
label define k3q20_lab  3  "Sometimes", add
label define k3q20_lab  4  "Never", add
label define k3q20_lab  .m "No valid response", add
label define k3q20_lab  .n "Not in universe", add
label define k3q20_lab  .l "Logical skip", add
label define k3q20_lab  .d "Suppressed for confidentiality", add
cap label values k3q20 k3q20_lab
label define k3q21b_lab  1  "Always"
label define k3q21b_lab  2  "Usually", add
label define k3q21b_lab  3  "Sometimes", add
label define k3q21b_lab  4  "Never", add
label define k3q21b_lab  .m "No valid response", add
label define k3q21b_lab  .n "Not in universe", add
label define k3q21b_lab  .l "Logical skip", add
label define k3q21b_lab  .d "Suppressed for confidentiality", add
cap label values k3q21b k3q21b_lab
label define k3q22_lab  1  "Always"
label define k3q22_lab  2  "Usually", add
label define k3q22_lab  3  "Sometimes", add
label define k3q22_lab  4  "Never", add
label define k3q22_lab  .m "No valid response", add
label define k3q22_lab  .n "Not in universe", add
label define k3q22_lab  .l "Logical skip", add
label define k3q22_lab  .d "Suppressed for confidentiality", add
cap label values k3q22 k3q22_lab
label define k3q25_lab  1  "Yes"
label define k3q25_lab  2  "No", add
label define k3q25_lab  .m "No valid response", add
label define k3q25_lab  .n "Not in universe", add
label define k3q25_lab  .l "Logical skip", add
label define k3q25_lab  .d "Suppressed for confidentiality", add
cap label values k3q25 k3q25_lab
label define k4q01_lab  1  "Yes"
label define k4q01_lab  2  "No", add
label define k4q01_lab  .m "No valid response", add
label define k4q01_lab  .n "Not in universe", add
label define k4q01_lab  .l "Logical skip", add
label define k4q01_lab  .d "Suppressed for confidentiality", add
cap label values k4q01 k4q01_lab
label define k4q02_r_lab  1  "Doctor's Office"
label define k4q02_r_lab  2  "Hospital Emergency Room", add
label define k4q02_r_lab  3  "Hospital Outpatient Department", add
label define k4q02_r_lab  4  "Clinic or Health Center", add
label define k4q02_r_lab  5  "Retail Store Clinic or 'Minute Clinic'", add
label define k4q02_r_lab  6  "School (Nurse's Office, Athletic Trainer's Office)", add
label define k4q02_r_lab  7  "Some other place", add
label define k4q02_r_lab  .m "No valid response", add
label define k4q02_r_lab  .n "Not in universe", add
label define k4q02_r_lab  .l "Logical skip", add
label define k4q02_r_lab  .d "Suppressed for confidentiality", add
cap label values k4q02_r k4q02_r_lab
label define k4q04_r_lab  1  "Yes, one person"
label define k4q04_r_lab  2  "Yes, more than one person", add
label define k4q04_r_lab  3  "No", add
label define k4q04_r_lab  .m "No valid response", add
label define k4q04_r_lab  .n "Not in universe", add
label define k4q04_r_lab  .l "Logical skip", add
label define k4q04_r_lab  .d "Suppressed for confidentiality", add
cap label values k4q04_r k4q04_r_lab
label define k4q20r_lab  1  "0 visits"
label define k4q20r_lab  2  "1 visit", add
label define k4q20r_lab  3  "2 or more visits", add
label define k4q20r_lab  .m "No valid response", add
label define k4q20r_lab  .n "Not in universe", add
label define k4q20r_lab  .l "Logical skip", add
label define k4q20r_lab  .d "Suppressed for confidentiality", add
cap label values k4q20r k4q20r_lab
label define k4q22_r_lab  1  "Yes"
label define k4q22_r_lab  2  "No, but this child needed to see a mental health professional", add
label define k4q22_r_lab  3  "No, this child did not need to see a mental health professional", add
label define k4q22_r_lab  .m "No valid response", add
label define k4q22_r_lab  .n "Not in universe", add
label define k4q22_r_lab  .l "Logical skip", add
label define k4q22_r_lab  .d "Suppressed for confidentiality", add
cap label values k4q22_r k4q22_r_lab
label define k4q23_lab  1  "Yes"
label define k4q23_lab  2  "No", add
label define k4q23_lab  .m "No valid response", add
label define k4q23_lab  .n "Not in universe", add
label define k4q23_lab  .l "Logical skip", add
label define k4q23_lab  .d "Suppressed for confidentiality", add
cap label values k4q23 k4q23_lab
label define k4q24_r_lab  1  "Yes"
label define k4q24_r_lab  2  "No, but this child needed to see a specialist", add
label define k4q24_r_lab  3  "No, this child did not need to see a specialist", add
label define k4q24_r_lab  .m "No valid response", add
label define k4q24_r_lab  .n "Not in universe", add
label define k4q24_r_lab  .l "Logical skip", add
label define k4q24_r_lab  .d "Suppressed for confidentiality", add
cap label values k4q24_r k4q24_r_lab
label define k4q26_lab  1  "Not difficult"
label define k4q26_lab  2  "Somewhat difficult", add
label define k4q26_lab  3  "Very difficult", add
label define k4q26_lab  4  "It was not possible to obtain care", add
label define k4q26_lab  .m "No valid response", add
label define k4q26_lab  .n "Not in universe", add
label define k4q26_lab  .l "Logical skip", add
label define k4q26_lab  .d "Suppressed for confidentiality", add
cap label values k4q26 k4q26_lab
label define k4q27_lab  1  "Yes"
label define k4q27_lab  2  "No", add
label define k4q27_lab  .m "No valid response", add
label define k4q27_lab  .n "Not in universe", add
label define k4q27_lab  .l "Logical skip", add
label define k4q27_lab  .d "Suppressed for confidentiality", add
cap label values k4q27 k4q27_lab
label define k4q28x01_lab  1  "selected"
label define k4q28x01_lab  2  "not selected", add
label define k4q28x01_lab  .m "No valid response", add
label define k4q28x01_lab  .n "Not in universe", add
label define k4q28x01_lab  .l "Logical skip", add
label define k4q28x01_lab  .d "Suppressed for confidentiality", add
cap label values k4q28x01 k4q28x01_lab
label define k4q28x02_lab  1  "selected"
label define k4q28x02_lab  2  "not selected", add
label define k4q28x02_lab  .m "No valid response", add
label define k4q28x02_lab  .n "Not in universe", add
label define k4q28x02_lab  .l "Logical skip", add
label define k4q28x02_lab  .d "Suppressed for confidentiality", add
cap label values k4q28x02 k4q28x02_lab
label define k4q28x03_lab  1  "selected"
label define k4q28x03_lab  2  "not selected", add
label define k4q28x03_lab  .m "No valid response", add
label define k4q28x03_lab  .n "Not in universe", add
label define k4q28x03_lab  .l "Logical skip", add
label define k4q28x03_lab  .d "Suppressed for confidentiality", add
cap label values k4q28x03 k4q28x03_lab
label define k4q28x04_lab  1  "selected"
label define k4q28x04_lab  2  "not selected", add
label define k4q28x04_lab  .m "No valid response", add
label define k4q28x04_lab  .n "Not in universe", add
label define k4q28x04_lab  .l "Logical skip", add
label define k4q28x04_lab  .d "Suppressed for confidentiality", add
cap label values k4q28x04 k4q28x04_lab
label define k4q28x05_lab  1  "selected"
label define k4q28x05_lab  2  "not selected", add
label define k4q28x05_lab  .m "No valid response", add
label define k4q28x05_lab  .n "Not in universe", add
label define k4q28x05_lab  .l "Logical skip", add
label define k4q28x05_lab  .d "Suppressed for confidentiality", add
cap label values k4q28x05 k4q28x05_lab
label define k4q28x_ear_lab  1  "selected"
label define k4q28x_ear_lab  2  "not selected", add
label define k4q28x_ear_lab  .m "No valid response", add
label define k4q28x_ear_lab  .n "Not in universe", add
label define k4q28x_ear_lab  .l "Logical skip", add
label define k4q28x_ear_lab  .d "Suppressed for confidentiality", add
cap label values k4q28x_ear k4q28x_ear_lab
label define k4q30_r_1_lab  1  "selected"
label define k4q30_r_1_lab  2  "not selected", add
label define k4q30_r_1_lab  .m "No valid response", add
label define k4q30_r_1_lab  .n "Not in universe", add
label define k4q30_r_1_lab  .l "Logical skip", add
label define k4q30_r_1_lab  .d "Suppressed for confidentiality", add
cap label values k4q30_r_1 k4q30_r_1_lab
label define k4q30_r_2_lab  1  "selected"
label define k4q30_r_2_lab  2  "not selected", add
label define k4q30_r_2_lab  .m "No valid response", add
label define k4q30_r_2_lab  .n "Not in universe", add
label define k4q30_r_2_lab  .l "Logical skip", add
label define k4q30_r_2_lab  .d "Suppressed for confidentiality", add
cap label values k4q30_r_2 k4q30_r_2_lab
label define k4q30_r_3_lab  1  "selected"
label define k4q30_r_3_lab  2  "not selected", add
label define k4q30_r_3_lab  .m "No valid response", add
label define k4q30_r_3_lab  .n "Not in universe", add
label define k4q30_r_3_lab  .l "Logical skip", add
label define k4q30_r_3_lab  .d "Suppressed for confidentiality", add
cap label values k4q30_r_3 k4q30_r_3_lab
label define k4q36_lab  1  "Yes"
label define k4q36_lab  2  "No", add
label define k4q36_lab  .m "No valid response", add
label define k4q36_lab  .n "Not in universe", add
label define k4q36_lab  .l "Logical skip", add
label define k4q36_lab  .d "Suppressed for confidentiality", add
cap label values k4q36 k4q36_lab
label define k4q37_lab  .m "No valid response"
label define k4q37_lab  .n "Not in universe", add
label define k4q37_lab  .l "Logical skip", add
label define k4q37_lab  .d "Suppressed for confidentiality", add
cap label values k4q37 k4q37_lab
label define k4q38_lab  1  "Yes"
label define k4q38_lab  2  "No", add
label define k4q38_lab  .m "No valid response", add
label define k4q38_lab  .n "Not in universe", add
label define k4q38_lab  .l "Logical skip", add
label define k4q38_lab  .d "Suppressed for confidentiality", add
cap label values k4q38 k4q38_lab
label define k5q10_lab  1  "Yes"
label define k5q10_lab  2  "No", add
label define k5q10_lab  .m "No valid response", add
label define k5q10_lab  .n "Not in universe", add
label define k5q10_lab  .l "Logical skip", add
label define k5q10_lab  .d "Suppressed for confidentiality", add
cap label values k5q10 k5q10_lab
label define k5q11_lab  1  "Not difficult"
label define k5q11_lab  2  "Somewhat difficult", add
label define k5q11_lab  3  "Very difficult", add
label define k5q11_lab  4  "It was not possible to get a referral", add
label define k5q11_lab  .m "No valid response", add
label define k5q11_lab  .n "Not in universe", add
label define k5q11_lab  .l "Logical skip", add
label define k5q11_lab  .d "Suppressed for confidentiality", add
cap label values k5q11 k5q11_lab
label define k5q20_r_lab  1  "Yes"
label define k5q20_r_lab  2  "No", add
label define k5q20_r_lab  3  "Did not see more than one health care provider in the PAST 12 MONTHS", add
label define k5q20_r_lab  .m "No valid response", add
label define k5q20_r_lab  .n "Not in universe", add
label define k5q20_r_lab  .l "Logical skip", add
label define k5q20_r_lab  .d "Suppressed for confidentiality", add
cap label values k5q20_r k5q20_r_lab
label define k5q21_lab  1  "Yes"
label define k5q21_lab  2  "No", add
label define k5q21_lab  .m "No valid response", add
label define k5q21_lab  .n "Not in universe", add
label define k5q21_lab  .l "Logical skip", add
label define k5q21_lab  .d "Suppressed for confidentiality", add
cap label values k5q21 k5q21_lab
label define k5q22_lab  1  "Usually"
label define k5q22_lab  2  "Sometimes", add
label define k5q22_lab  3  "Never", add
label define k5q22_lab  .m "No valid response", add
label define k5q22_lab  .n "Not in universe", add
label define k5q22_lab  .l "Logical skip", add
label define k5q22_lab  .d "Suppressed for confidentiality", add
cap label values k5q22 k5q22_lab
label define k5q30_lab  1  "Very Satisfied"
label define k5q30_lab  2  "Somewhat satisfied", add
label define k5q30_lab  3  "Somewhat dissatisfied", add
label define k5q30_lab  4  "Very dissatisfied", add
label define k5q30_lab  .m "No valid response", add
label define k5q30_lab  .n "Not in universe", add
label define k5q30_lab  .l "Logical skip", add
label define k5q30_lab  .d "Suppressed for confidentiality", add
cap label values k5q30 k5q30_lab
label define k5q31_r_lab  1  "Yes"
label define k5q31_r_lab  2  "No", add
label define k5q31_r_lab  3  "Did not need health care provider to communicate with these providers", add
label define k5q31_r_lab  .m "No valid response", add
label define k5q31_r_lab  .n "Not in universe", add
label define k5q31_r_lab  .l "Logical skip", add
label define k5q31_r_lab  .d "Suppressed for confidentiality", add
cap label values k5q31_r k5q31_r_lab
label define k5q32_lab  1  "Very Satisfied"
label define k5q32_lab  2  "Somewhat satisfied", add
label define k5q32_lab  3  "Somewhat dissatisfied", add
label define k5q32_lab  4  "Very dissatisfied", add
label define k5q32_lab  .m "No valid response", add
label define k5q32_lab  .n "Not in universe", add
label define k5q32_lab  .l "Logical skip", add
label define k5q32_lab  .d "Suppressed for confidentiality", add
cap label values k5q32 k5q32_lab
label define k5q40_lab  1  "Always"
label define k5q40_lab  2  "Usually", add
label define k5q40_lab  3  "Sometimes", add
label define k5q40_lab  4  "Never", add
label define k5q40_lab  .m "No valid response", add
label define k5q40_lab  .n "Not in universe", add
label define k5q40_lab  .l "Logical skip", add
label define k5q40_lab  .d "Suppressed for confidentiality", add
cap label values k5q40 k5q40_lab
label define k5q41_lab  1  "Always"
label define k5q41_lab  2  "Usually", add
label define k5q41_lab  3  "Sometimes", add
label define k5q41_lab  4  "Never", add
label define k5q41_lab  .m "No valid response", add
label define k5q41_lab  .n "Not in universe", add
label define k5q41_lab  .l "Logical skip", add
label define k5q41_lab  .d "Suppressed for confidentiality", add
cap label values k5q41 k5q41_lab
label define k5q42_lab  1  "Always"
label define k5q42_lab  2  "Usually", add
label define k5q42_lab  3  "Sometimes", add
label define k5q42_lab  4  "Never", add
label define k5q42_lab  .m "No valid response", add
label define k5q42_lab  .n "Not in universe", add
label define k5q42_lab  .l "Logical skip", add
label define k5q42_lab  .d "Suppressed for confidentiality", add
cap label values k5q42 k5q42_lab
label define k5q43_lab  1  "Always"
label define k5q43_lab  2  "Usually", add
label define k5q43_lab  3  "Sometimes", add
label define k5q43_lab  4  "Never", add
label define k5q43_lab  .m "No valid response", add
label define k5q43_lab  .n "Not in universe", add
label define k5q43_lab  .l "Logical skip", add
label define k5q43_lab  .d "Suppressed for confidentiality", add
cap label values k5q43 k5q43_lab
label define k5q44_lab  1  "Always"
label define k5q44_lab  2  "Usually", add
label define k5q44_lab  3  "Sometimes", add
label define k5q44_lab  4  "Never", add
label define k5q44_lab  .m "No valid response", add
label define k5q44_lab  .n "Not in universe", add
label define k5q44_lab  .l "Logical skip", add
label define k5q44_lab  .d "Suppressed for confidentiality", add
cap label values k5q44 k5q44_lab
label define k6q10_lab  1  "Yes"
label define k6q10_lab  2  "No", add
label define k6q10_lab  .m "No valid response", add
label define k6q10_lab  .n "Not in universe", add
label define k6q10_lab  .l "Logical skip", add
label define k6q10_lab  .d "Suppressed for confidentiality", add
cap label values k6q10 k6q10_lab
label define k6q12_lab  1  "Yes"
label define k6q12_lab  2  "No", add
label define k6q12_lab  .m "No valid response", add
label define k6q12_lab  .n "Not in universe", add
label define k6q12_lab  .l "Logical skip", add
label define k6q12_lab  .d "Suppressed for confidentiality", add
cap label values k6q12 k6q12_lab
label define k6q13a_lab  1  "selected"
label define k6q13a_lab  2  "not selected", add
label define k6q13a_lab  .m "No valid response", add
label define k6q13a_lab  .n "Not in universe", add
label define k6q13a_lab  .l "Logical skip", add
label define k6q13a_lab  .d "Suppressed for confidentiality", add
cap label values k6q13a k6q13a_lab
label define k6q13b_lab  1  "selected"
label define k6q13b_lab  2  "not selected", add
label define k6q13b_lab  .m "No valid response", add
label define k6q13b_lab  .n "Not in universe", add
label define k6q13b_lab  .l "Logical skip", add
label define k6q13b_lab  .d "Suppressed for confidentiality", add
cap label values k6q13b k6q13b_lab
label define k6q14a_lab  1  "selected"
label define k6q14a_lab  2  "not selected", add
label define k6q14a_lab  .m "No valid response", add
label define k6q14a_lab  .n "Not in universe", add
label define k6q14a_lab  .l "Logical skip", add
label define k6q14a_lab  .d "Suppressed for confidentiality", add
cap label values k6q14a k6q14a_lab
label define k6q14b_lab  1  "selected"
label define k6q14b_lab  2  "not selected", add
label define k6q14b_lab  .m "No valid response", add
label define k6q14b_lab  .n "Not in universe", add
label define k6q14b_lab  .l "Logical skip", add
label define k6q14b_lab  .d "Suppressed for confidentiality", add
cap label values k6q14b k6q14b_lab
label define k6q15_lab  1  "Yes"
label define k6q15_lab  2  "No", add
label define k6q15_lab  .m "No valid response", add
label define k6q15_lab  .n "Not in universe", add
label define k6q15_lab  .l "Logical skip", add
label define k6q15_lab  .d "Suppressed for confidentiality", add
cap label values k6q15 k6q15_lab
label define k6q20_lab  1  "Yes"
label define k6q20_lab  2  "No", add
label define k6q20_lab  .m "No valid response", add
label define k6q20_lab  .n "Not in universe", add
label define k6q20_lab  .l "Logical skip", add
label define k6q20_lab  .d "Suppressed for confidentiality", add
cap label values k6q20 k6q20_lab
label define k6q27_lab  1  "Yes"
label define k6q27_lab  2  "No", add
label define k6q27_lab  .m "No valid response", add
label define k6q27_lab  .n "Not in universe", add
label define k6q27_lab  .l "Logical skip", add
label define k6q27_lab  .d "Suppressed for confidentiality", add
cap label values k6q27 k6q27_lab
label define k6q40_lab  1  "Yes"
label define k6q40_lab  2  "No", add
label define k6q40_lab  .m "No valid response", add
label define k6q40_lab  .n "Not in universe", add
label define k6q40_lab  .l "Logical skip", add
label define k6q40_lab  .d "Suppressed for confidentiality", add
cap label values k6q40 k6q40_lab
label define k6q41r_still_lab  1  "selected"
label define k6q41r_still_lab  2  "not selected", add
label define k6q41r_still_lab  .m "No valid response", add
label define k6q41r_still_lab  .n "Not in universe", add
label define k6q41r_still_lab  .l "Logical skip", add
label define k6q41r_still_lab  .d "Suppressed for confidentiality", add
cap label values k6q41r_still k6q41r_still_lab
label define k6q42r_never_lab  1  "selected"
label define k6q42r_never_lab  2  "not selected", add
label define k6q42r_never_lab  .m "No valid response", add
label define k6q42r_never_lab  .n "Not in universe", add
label define k6q42r_never_lab  .l "Logical skip", add
label define k6q42r_never_lab  .d "Suppressed for confidentiality", add
cap label values k6q42r_never k6q42r_never_lab
label define k6q43r_never_lab  1  "selected"
label define k6q43r_never_lab  2  "not selected", add
label define k6q43r_never_lab  .m "No valid response", add
label define k6q43r_never_lab  .n "Not in universe", add
label define k6q43r_never_lab  .l "Logical skip", add
label define k6q43r_never_lab  .d "Suppressed for confidentiality", add
cap label values k6q43r_never k6q43r_never_lab
label define k6q60_r_lab  1  "0 days"
label define k6q60_r_lab  2  "1-3 days", add
label define k6q60_r_lab  3  "4-6 days", add
label define k6q60_r_lab  4  "Every day", add
label define k6q60_r_lab  .m "No valid response", add
label define k6q60_r_lab  .n "Not in universe", add
label define k6q60_r_lab  .l "Logical skip", add
label define k6q60_r_lab  .d "Suppressed for confidentiality", add
cap label values k6q60_r k6q60_r_lab
label define k6q61_r_lab  1  "0 days"
label define k6q61_r_lab  2  "1-3 days", add
label define k6q61_r_lab  3  "4-6 days", add
label define k6q61_r_lab  4  "Every day", add
label define k6q61_r_lab  .m "No valid response", add
label define k6q61_r_lab  .n "Not in universe", add
label define k6q61_r_lab  .l "Logical skip", add
label define k6q61_r_lab  .d "Suppressed for confidentiality", add
cap label values k6q61_r k6q61_r_lab
label define k6q70_r_lab  1  "Always"
label define k6q70_r_lab  2  "Usually", add
label define k6q70_r_lab  3  "Sometimes", add
label define k6q70_r_lab  4  "Never", add
label define k6q70_r_lab  .m "No valid response", add
label define k6q70_r_lab  .n "Not in universe", add
label define k6q70_r_lab  .l "Logical skip", add
label define k6q70_r_lab  .d "Suppressed for confidentiality", add
cap label values k6q70_r k6q70_r_lab
label define k6q71_r_lab  1  "Always"
label define k6q71_r_lab  2  "Usually", add
label define k6q71_r_lab  3  "Sometimes", add
label define k6q71_r_lab  4  "Never", add
label define k6q71_r_lab  .m "No valid response", add
label define k6q71_r_lab  .n "Not in universe", add
label define k6q71_r_lab  .l "Logical skip", add
label define k6q71_r_lab  .d "Suppressed for confidentiality", add
cap label values k6q71_r k6q71_r_lab
label define k6q72_r_lab  1  "Always"
label define k6q72_r_lab  2  "Usually", add
label define k6q72_r_lab  3  "Sometimes", add
label define k6q72_r_lab  4  "Never", add
label define k6q72_r_lab  .m "No valid response", add
label define k6q72_r_lab  .n "Not in universe", add
label define k6q72_r_lab  .l "Logical skip", add
label define k6q72_r_lab  .d "Suppressed for confidentiality", add
cap label values k6q72_r k6q72_r_lab
label define k6q73_r_lab  1  "Always"
label define k6q73_r_lab  2  "Usually", add
label define k6q73_r_lab  3  "Sometimes", add
label define k6q73_r_lab  4  "Never", add
label define k6q73_r_lab  .m "No valid response", add
label define k6q73_r_lab  .n "Not in universe", add
label define k6q73_r_lab  .l "Logical skip", add
label define k6q73_r_lab  .d "Suppressed for confidentiality", add
cap label values k6q73_r k6q73_r_lab
label define k7q02r_r_lab  1  "No missed school days"
label define k7q02r_r_lab  2  "1 - 3 days", add
label define k7q02r_r_lab  3  "4 - 6 days", add
label define k7q02r_r_lab  4  "7 - 10 days", add
label define k7q02r_r_lab  5  "11 or more days", add
label define k7q02r_r_lab  6  "This child was not enrolled in school", add
label define k7q02r_r_lab  .m "No valid response", add
label define k7q02r_r_lab  .n "Not in universe", add
label define k7q02r_r_lab  .l "Logical skip", add
label define k7q02r_r_lab  .d "Suppressed for confidentiality", add
cap label values k7q02r_r k7q02r_r_lab
label define k7q04r_r_lab  1  "None"
label define k7q04r_r_lab  2  "1 time", add
label define k7q04r_r_lab  3  "2 or more times", add
label define k7q04r_r_lab  .m "No valid response", add
label define k7q04r_r_lab  .n "Not in universe", add
label define k7q04r_r_lab  .l "Logical skip", add
label define k7q04r_r_lab  .d "Suppressed for confidentiality", add
cap label values k7q04r_r k7q04r_r_lab
label define k7q30_lab  1  "Yes"
label define k7q30_lab  2  "No", add
label define k7q30_lab  .m "No valid response", add
label define k7q30_lab  .n "Not in universe", add
label define k7q30_lab  .l "Logical skip", add
label define k7q30_lab  .d "Suppressed for confidentiality", add
cap label values k7q30 k7q30_lab
label define k7q31_lab  1  "Yes"
label define k7q31_lab  2  "No", add
label define k7q31_lab  .m "No valid response", add
label define k7q31_lab  .n "Not in universe", add
label define k7q31_lab  .l "Logical skip", add
label define k7q31_lab  .d "Suppressed for confidentiality", add
cap label values k7q31 k7q31_lab
label define k7q32_lab  1  "Yes"
label define k7q32_lab  2  "No", add
label define k7q32_lab  .m "No valid response", add
label define k7q32_lab  .n "Not in universe", add
label define k7q32_lab  .l "Logical skip", add
label define k7q32_lab  .d "Suppressed for confidentiality", add
cap label values k7q32 k7q32_lab
label define k7q33_lab  1  "Always"
label define k7q33_lab  2  "Usually", add
label define k7q33_lab  3  "Sometimes", add
label define k7q33_lab  4  "Rarely", add
label define k7q33_lab  5  "Never", add
label define k7q33_lab  .m "No valid response", add
label define k7q33_lab  .n "Not in universe", add
label define k7q33_lab  .l "Logical skip", add
label define k7q33_lab  .d "Suppressed for confidentiality", add
cap label values k7q33 k7q33_lab
label define k7q37_lab  1  "Yes"
label define k7q37_lab  2  "No", add
label define k7q37_lab  .m "No valid response", add
label define k7q37_lab  .n "Not in universe", add
label define k7q37_lab  .l "Logical skip", add
label define k7q37_lab  .d "Suppressed for confidentiality", add
cap label values k7q37 k7q37_lab
label define k7q38_lab  1  "Yes"
label define k7q38_lab  2  "No", add
label define k7q38_lab  .m "No valid response", add
label define k7q38_lab  .n "Not in universe", add
label define k7q38_lab  .l "Logical skip", add
label define k7q38_lab  .d "Suppressed for confidentiality", add
cap label values k7q38 k7q38_lab
label define k7q70_r_lab  1  "Always"
label define k7q70_r_lab  2  "Usually", add
label define k7q70_r_lab  3  "Sometimes", add
label define k7q70_r_lab  4  "Never", add
label define k7q70_r_lab  .m "No valid response", add
label define k7q70_r_lab  .n "Not in universe", add
label define k7q70_r_lab  .l "Logical skip", add
label define k7q70_r_lab  .d "Suppressed for confidentiality", add
cap label values k7q70_r k7q70_r_lab
label define k7q82_r_lab  1  "Always"
label define k7q82_r_lab  2  "Usually", add
label define k7q82_r_lab  3  "Sometimes", add
label define k7q82_r_lab  4  "Never", add
label define k7q82_r_lab  .m "No valid response", add
label define k7q82_r_lab  .n "Not in universe", add
label define k7q82_r_lab  .l "Logical skip", add
label define k7q82_r_lab  .d "Suppressed for confidentiality", add
cap label values k7q82_r k7q82_r_lab
label define k7q83_r_lab  1  "Always"
label define k7q83_r_lab  2  "Usually", add
label define k7q83_r_lab  3  "Sometimes", add
label define k7q83_r_lab  4  "Never", add
label define k7q83_r_lab  .m "No valid response", add
label define k7q83_r_lab  .n "Not in universe", add
label define k7q83_r_lab  .l "Logical skip", add
label define k7q83_r_lab  .d "Suppressed for confidentiality", add
cap label values k7q83_r k7q83_r_lab
label define k7q84_r_lab  1  "Always"
label define k7q84_r_lab  2  "Usually", add
label define k7q84_r_lab  3  "Sometimes", add
label define k7q84_r_lab  4  "Never", add
label define k7q84_r_lab  .m "No valid response", add
label define k7q84_r_lab  .n "Not in universe", add
label define k7q84_r_lab  .l "Logical skip", add
label define k7q84_r_lab  .d "Suppressed for confidentiality", add
cap label values k7q84_r k7q84_r_lab
label define k7q85_r_lab  1  "Always"
label define k7q85_r_lab  2  "Usually", add
label define k7q85_r_lab  3  "Sometimes", add
label define k7q85_r_lab  4  "Never", add
label define k7q85_r_lab  .m "No valid response", add
label define k7q85_r_lab  .n "Not in universe", add
label define k7q85_r_lab  .l "Logical skip", add
label define k7q85_r_lab  .d "Suppressed for confidentiality", add
cap label values k7q85_r k7q85_r_lab
label define k8q11_lab  1  "0 days"
label define k8q11_lab  2  "1-3 days", add
label define k8q11_lab  3  "4-6 days", add
label define k8q11_lab  4  "Every day", add
label define k8q11_lab  .m "No valid response", add
label define k8q11_lab  .n "Not in universe", add
label define k8q11_lab  .l "Logical skip", add
label define k8q11_lab  .d "Suppressed for confidentiality", add
cap label values k8q11 k8q11_lab
label define k8q21_lab  1  "Very well"
label define k8q21_lab  2  "Somewhat well", add
label define k8q21_lab  3  "Not very well", add
label define k8q21_lab  4  "Not well at all", add
label define k8q21_lab  .m "No valid response", add
label define k8q21_lab  .n "Not in universe", add
label define k8q21_lab  .l "Logical skip", add
label define k8q21_lab  .d "Suppressed for confidentiality", add
cap label values k8q21 k8q21_lab
label define k8q30_lab  1  "Very well"
label define k8q30_lab  2  "Somewhat well", add
label define k8q30_lab  3  "Not very well", add
label define k8q30_lab  4  "Not well at all", add
label define k8q30_lab  .m "No valid response", add
label define k8q30_lab  .n "Not in universe", add
label define k8q30_lab  .l "Logical skip", add
label define k8q30_lab  .d "Suppressed for confidentiality", add
cap label values k8q30 k8q30_lab
label define k8q31_lab  1  "Never"
label define k8q31_lab  2  "Rarely", add
label define k8q31_lab  3  "Sometimes", add
label define k8q31_lab  4  "Usually", add
label define k8q31_lab  5  "Always", add
label define k8q31_lab  .m "No valid response", add
label define k8q31_lab  .n "Not in universe", add
label define k8q31_lab  .l "Logical skip", add
label define k8q31_lab  .d "Suppressed for confidentiality", add
cap label values k8q31 k8q31_lab
label define k8q32_lab  1  "Never"
label define k8q32_lab  2  "Rarely", add
label define k8q32_lab  3  "Sometimes", add
label define k8q32_lab  4  "Usually", add
label define k8q32_lab  5  "Always", add
label define k8q32_lab  .m "No valid response", add
label define k8q32_lab  .n "Not in universe", add
label define k8q32_lab  .l "Logical skip", add
label define k8q32_lab  .d "Suppressed for confidentiality", add
cap label values k8q32 k8q32_lab
label define k8q34_lab  1  "Never"
label define k8q34_lab  2  "Rarely", add
label define k8q34_lab  3  "Sometimes", add
label define k8q34_lab  4  "Usually", add
label define k8q34_lab  5  "Always", add
label define k8q34_lab  .m "No valid response", add
label define k8q34_lab  .n "Not in universe", add
label define k8q34_lab  .l "Logical skip", add
label define k8q34_lab  .d "Suppressed for confidentiality", add
cap label values k8q34 k8q34_lab
label define k8q35_lab  1  "Yes"
label define k8q35_lab  2  "No", add
label define k8q35_lab  .m "No valid response", add
label define k8q35_lab  .n "Not in universe", add
label define k8q35_lab  .l "Logical skip", add
label define k8q35_lab  .d "Suppressed for confidentiality", add
cap label values k8q35 k8q35_lab
label define k9q40_lab  1  "Yes"
label define k9q40_lab  2  "No", add
label define k9q40_lab  .m "No valid response", add
label define k9q40_lab  .n "Not in universe", add
label define k9q40_lab  .l "Logical skip", add
label define k9q40_lab  .d "Suppressed for confidentiality", add
cap label values k9q40 k9q40_lab
label define k9q41_lab  1  "Yes"
label define k9q41_lab  2  "No", add
label define k9q41_lab  .m "No valid response", add
label define k9q41_lab  .n "Not in universe", add
label define k9q41_lab  .l "Logical skip", add
label define k9q41_lab  .d "Suppressed for confidentiality", add
cap label values k9q41 k9q41_lab
label define k9q96_lab  1  "Yes"
label define k9q96_lab  2  "No", add
label define k9q96_lab  .m "No valid response", add
label define k9q96_lab  .n "Not in universe", add
label define k9q96_lab  .l "Logical skip", add
label define k9q96_lab  .d "Suppressed for confidentiality", add
cap label values k9q96 k9q96_lab
label define keepinsadult_lab  1  "Yes"
label define keepinsadult_lab  2  "No", add
label define keepinsadult_lab  .m "No valid response", add
label define keepinsadult_lab  .n "Not in universe", add
label define keepinsadult_lab  .l "Logical skip", add
label define keepinsadult_lab  .d "Suppressed for confidentiality", add
cap label values keepinsadult keepinsadult_lab
label define liveusa_mo_lab  .m "No valid response"
label define liveusa_mo_lab  .n "Not in universe", add
label define liveusa_mo_lab  .l "Logical skip", add
label define liveusa_mo_lab  .d "Suppressed for confidentiality", add
cap label values liveusa_mo liveusa_mo_lab
label define liveusa_yr_lab  .m "No valid response"
label define liveusa_yr_lab  .n "Not in universe", add
label define liveusa_yr_lab  .l "Logical skip", add
label define liveusa_yr_lab  .d "Suppressed for confidentiality", add
cap label values liveusa_yr liveusa_yr_lab
label define makefriend_lab  3  "A lot of difficulty"
label define makefriend_lab  2  "A little difficulty", add
label define makefriend_lab  1  "No difficulty", add
label define makefriend_lab  .m "No valid response", add
label define makefriend_lab  .n "Not in universe", add
label define makefriend_lab  .l "Logical skip", add
label define makefriend_lab  .d "Suppressed for confidentiality", add
cap label values makefriend makefriend_lab
label define medhistory_lab  1  "Yes"
label define medhistory_lab  2  "No", add
label define medhistory_lab  .m "No valid response", add
label define medhistory_lab  .n "Not in universe", add
label define medhistory_lab  .l "Logical skip", add
label define medhistory_lab  .d "Suppressed for confidentiality", add
cap label values medhistory medhistory_lab
label define memorycond_lab  1  "Yes"
label define memorycond_lab  2  "No", add
label define memorycond_lab  .m "No valid response", add
label define memorycond_lab  .n "Not in universe", add
label define memorycond_lab  .l "Logical skip", add
label define memorycond_lab  .d "Suppressed for confidentiality", add
cap label values memorycond memorycond_lab
label define menbevcov_lab  1  "Always"
label define menbevcov_lab  2  "Usually", add
label define menbevcov_lab  3  "Sometimes", add
label define menbevcov_lab  4  "Never", add
label define menbevcov_lab  5  "This child does not use mental or behavioral health services", add
label define menbevcov_lab  .m "No valid response", add
label define menbevcov_lab  .n "Not in universe", add
label define menbevcov_lab  .l "Logical skip", add
label define menbevcov_lab  .d "Suppressed for confidentiality", add
cap label values menbevcov menbevcov_lab
label define metro_yn_lab  1  "Metropolitan Statistical Area"
label define metro_yn_lab  2  "Not Metropolitan Statistical Area", add
label define metro_yn_lab  .m "No valid response", add
label define metro_yn_lab  .n "Not in universe", add
label define metro_yn_lab  .l "Logical skip", add
label define metro_yn_lab  .d "Suppressed for confidentiality", add
cap label values metro_yn metro_yn_lab
label define missmortgage_lab  1  "Yes"
label define missmortgage_lab  2  "No", add
label define missmortgage_lab  3  "Don't Know", add
label define missmortgage_lab  .m "No valid response", add
label define missmortgage_lab  .n "Not in universe", add
label define missmortgage_lab  .l "Logical skip", add
label define missmortgage_lab  .d "Suppressed for confidentiality", add
cap label values missmortgage missmortgage_lab
label define momage_lab  .m "No valid response"
label define momage_lab  .n "Not in universe", add
label define momage_lab  .l "Logical skip", add
label define momage_lab  .d "Suppressed for confidentiality", add
cap label values momage momage_lab
label define mpc_yn_lab  1  "Metropolitan Principal City"
label define mpc_yn_lab  2  "Not Metropolitan Principal City", add
label define mpc_yn_lab  .m "No valid response", add
label define mpc_yn_lab  .n "Not in universe", add
label define mpc_yn_lab  .l "Logical skip", add
label define mpc_yn_lab  .d "Suppressed for confidentiality", add
cap label values mpc_yn mpc_yn_lab
label define nameemotions_lab  1  "Always"
label define nameemotions_lab  2  "Most of the time", add
label define nameemotions_lab  3  "About half the time", add
label define nameemotions_lab  4  "Sometimes", add
label define nameemotions_lab  5  "Never", add
label define nameemotions_lab  .m "No valid response", add
label define nameemotions_lab  .n "Not in universe", add
label define nameemotions_lab  .l "Logical skip", add
label define nameemotions_lab  .d "Suppressed for confidentiality", add
cap label values nameemotions nameemotions_lab
label define notelig_lab  1  "Yes"
label define notelig_lab  2  "No", add
label define notelig_lab  .m "No valid response", add
label define notelig_lab  .n "Not in universe", add
label define notelig_lab  .l "Logical skip", add
label define notelig_lab  .d "Suppressed for confidentiality", add
cap label values notelig notelig_lab
label define notopen_lab  1  "Yes"
label define notopen_lab  2  "No", add
label define notopen_lab  .m "No valid response", add
label define notopen_lab  .n "Not in universe", add
label define notopen_lab  .l "Logical skip", add
label define notopen_lab  .d "Suppressed for confidentiality", add
cap label values notopen notopen_lab
label define oneword_lab  1  "Yes"
label define oneword_lab  2  "No", add
label define oneword_lab  .m "No valid response", add
label define oneword_lab  .n "Not in universe", add
label define oneword_lab  .l "Logical skip", add
label define oneword_lab  .d "Suppressed for confidentiality", add
cap label values oneword oneword_lab
label define outdoorswkday_lab  1  "Less than 1 hour per day"
label define outdoorswkday_lab  2  "1 hour per day", add
label define outdoorswkday_lab  3  "2 hours per day", add
label define outdoorswkday_lab  4  "3 hours per day", add
label define outdoorswkday_lab  5  "4 or more hours per day", add
label define outdoorswkday_lab  .m "No valid response", add
label define outdoorswkday_lab  .n "Not in universe", add
label define outdoorswkday_lab  .l "Logical skip", add
label define outdoorswkday_lab  .d "Suppressed for confidentiality", add
cap label values outdoorswkday outdoorswkday_lab
label define outdoorswkend_lab  1  "Less than 1 hour per day"
label define outdoorswkend_lab  2  "1 hour per day", add
label define outdoorswkend_lab  3  "2 hours per day", add
label define outdoorswkend_lab  4  "3 hours per day", add
label define outdoorswkend_lab  5  "4 or more hours per day", add
label define outdoorswkend_lab  .m "No valid response", add
label define outdoorswkend_lab  .n "Not in universe", add
label define outdoorswkend_lab  .l "Logical skip", add
label define outdoorswkend_lab  .d "Suppressed for confidentiality", add
cap label values outdoorswkend outdoorswkend_lab
label define overweight_lab  1  "Yes"
label define overweight_lab  2  "No", add
label define overweight_lab  .m "No valid response", add
label define overweight_lab  .n "Not in universe", add
label define overweight_lab  .l "Logical skip", add
label define overweight_lab  .d "Suppressed for confidentiality", add
cap label values overweight overweight_lab
label define physactiv_lab  1  "0 days"
label define physactiv_lab  2  "1 - 3 days", add
label define physactiv_lab  3  "4 - 6 days", add
label define physactiv_lab  4  "Every day", add
label define physactiv_lab  .m "No valid response", add
label define physactiv_lab  .n "Not in universe", add
label define physactiv_lab  .l "Logical skip", add
label define physactiv_lab  .d "Suppressed for confidentiality", add
cap label values physactiv physactiv_lab
label define physicalpain_lab  1  "Yes"
label define physicalpain_lab  2  "No", add
label define physicalpain_lab  .m "No valid response", add
label define physicalpain_lab  .n "Not in universe", add
label define physicalpain_lab  .l "Logical skip", add
label define physicalpain_lab  .d "Suppressed for confidentiality", add
cap label values physicalpain physicalpain_lab
label define placeslived_lab  1  "0 - 2 places lived"
label define placeslived_lab  2  "3 or more places lived", add
label define placeslived_lab  .m "No valid response", add
label define placeslived_lab  .n "Not in universe", add
label define placeslived_lab  .l "Logical skip", add
label define placeslived_lab  .d "Suppressed for confidentiality", add
cap label values placeslived placeslived_lab
label define planneeds_r_lab  1  "Yes"
label define planneeds_r_lab  2  "No", add
label define planneeds_r_lab  3  "No, this child already sees providers who treat adults", add
label define planneeds_r_lab  .m "No valid response", add
label define planneeds_r_lab  .n "Not in universe", add
label define planneeds_r_lab  .l "Logical skip", add
label define planneeds_r_lab  .d "Suppressed for confidentiality", add
cap label values planneeds_r planneeds_r_lab
label define playwell_lab  1  "Always"
label define playwell_lab  2  "Most of the time", add
label define playwell_lab  3  "About half the time", add
label define playwell_lab  4  "Sometimes", add
label define playwell_lab  5  "Never", add
label define playwell_lab  .m "No valid response", add
label define playwell_lab  .n "Not in universe", add
label define playwell_lab  .l "Logical skip", add
label define playwell_lab  .d "Suppressed for confidentiality", add
cap label values playwell playwell_lab
label define point_lab  1  "Yes"
label define point_lab  2  "No", add
label define point_lab  .m "No valid response", add
label define point_lab  .n "Not in universe", add
label define point_lab  .l "Logical skip", add
label define point_lab  .d "Suppressed for confidentiality", add
cap label values point point_lab
label define poschoice_lab  1  "Yes"
label define poschoice_lab  2  "No", add
label define poschoice_lab  3  "Don't Know", add
label define poschoice_lab  .m "No valid response", add
label define poschoice_lab  .n "Not in universe", add
label define poschoice_lab  .l "Logical skip", add
label define poschoice_lab  .d "Suppressed for confidentiality", add
cap label values poschoice poschoice_lab
label define raiseconc_lab  1  "Always"
label define raiseconc_lab  2  "Usually", add
label define raiseconc_lab  3  "Sometimes", add
label define raiseconc_lab  4  "Never", add
label define raiseconc_lab  .m "No valid response", add
label define raiseconc_lab  .n "Not in universe", add
label define raiseconc_lab  .l "Logical skip", add
label define raiseconc_lab  .d "Suppressed for confidentiality", add
cap label values raiseconc raiseconc_lab
label define readonedigit_lab  1  "Always"
label define readonedigit_lab  2  "Most of the time", add
label define readonedigit_lab  3  "About half the time", add
label define readonedigit_lab  4  "Sometimes", add
label define readonedigit_lab  5  "Never", add
label define readonedigit_lab  .m "No valid response", add
label define readonedigit_lab  .n "Not in universe", add
label define readonedigit_lab  .l "Logical skip", add
label define readonedigit_lab  .d "Suppressed for confidentiality", add
cap label values readonedigit readonedigit_lab
label define receivecopy_lab  1  "Yes"
label define receivecopy_lab  2  "No", add
label define receivecopy_lab  .m "No valid response", add
label define receivecopy_lab  .n "Not in universe", add
label define receivecopy_lab  .l "Logical skip", add
label define receivecopy_lab  .d "Suppressed for confidentiality", add
cap label values receivecopy receivecopy_lab
label define recevalfasd_lab  1  "Yes"
label define recevalfasd_lab  2  "No", add
label define recevalfasd_lab  3  "Don't Know", add
label define recevalfasd_lab  .m "No valid response", add
label define recevalfasd_lab  .n "Not in universe", add
label define recevalfasd_lab  .l "Logical skip", add
label define recevalfasd_lab  .d "Suppressed for confidentiality", add
cap label values recevalfasd recevalfasd_lab
label define recogabc_lab  1  "All of them"
label define recogabc_lab  2  "Most of them", add
label define recogabc_lab  3  "About half of them", add
label define recogabc_lab  4  "Some of them", add
label define recogabc_lab  5  "None of them", add
label define recogabc_lab  .m "No valid response", add
label define recogabc_lab  .n "Not in universe", add
label define recogabc_lab  .l "Logical skip", add
label define recogabc_lab  .d "Suppressed for confidentiality", add
cap label values recogabc recogabc_lab
label define recogbegin_lab  1  "Always"
label define recogbegin_lab  2  "Most of the time", add
label define recogbegin_lab  3  "About half the time", add
label define recogbegin_lab  4  "Sometimes", add
label define recogbegin_lab  5  "Never", add
label define recogbegin_lab  .m "No valid response", add
label define recogbegin_lab  .n "Not in universe", add
label define recogbegin_lab  .l "Logical skip", add
label define recogbegin_lab  .d "Suppressed for confidentiality", add
cap label values recogbegin recogbegin_lab
label define repeated_lab  1  "Yes"
label define repeated_lab  2  "No", add
label define repeated_lab  .m "No valid response", add
label define repeated_lab  .n "Not in universe", add
label define repeated_lab  .l "Logical skip", add
label define repeated_lab  .d "Suppressed for confidentiality", add
cap label values repeated repeated_lab
label define rhymeword_r_lab  1  "This child cannot rhyme"
label define rhymeword_r_lab  2  "Not well", add
label define rhymeword_r_lab  3  "Somewhat well", add
label define rhymeword_r_lab  4  "Very well", add
label define rhymeword_r_lab  .m "No valid response", add
label define rhymeword_r_lab  .n "Not in universe", add
label define rhymeword_r_lab  .l "Logical skip", add
label define rhymeword_r_lab  .d "Suppressed for confidentiality", add
cap label values rhymeword_r rhymeword_r_lab
label define s4q01_lab  1  "Yes"
label define s4q01_lab  2  "No", add
label define s4q01_lab  .m "No valid response", add
label define s4q01_lab  .n "Not in universe", add
label define s4q01_lab  .l "Logical skip", add
label define s4q01_lab  .d "Suppressed for confidentiality", add
cap label values s4q01 s4q01_lab
label define s9q34_lab  1  "Yes"
label define s9q34_lab  2  "No", add
label define s9q34_lab  .m "No valid response", add
label define s9q34_lab  .n "Not in universe", add
label define s9q34_lab  .l "Logical skip", add
label define s9q34_lab  .d "Suppressed for confidentiality", add
cap label values s9q34 s9q34_lab
label define samesound_lab  1  "Always"
label define samesound_lab  2  "Most of the time", add
label define samesound_lab  3  "About half the time", add
label define samesound_lab  4  "Sometimes", add
label define samesound_lab  5  "Never", add
label define samesound_lab  .m "No valid response", add
label define samesound_lab  .n "Not in universe", add
label define samesound_lab  .l "Logical skip", add
label define samesound_lab  .d "Suppressed for confidentiality", add
cap label values samesound samesound_lab
label define sc_age_lt10_lab  1  "LT 10 Months Old"
label define sc_age_lt10_lab  2  "GE 10 Months Old", add
label define sc_age_lt10_lab  .m "No valid response", add
label define sc_age_lt10_lab  .n "Not in universe", add
label define sc_age_lt10_lab  .l "Logical skip", add
label define sc_age_lt10_lab  .d "Suppressed for confidentiality", add
cap label values sc_age_lt10 sc_age_lt10_lab
label define sc_age_lt4_lab  1  "LT 4 Months Old"
label define sc_age_lt4_lab  2  "GE 4 Months Old", add
label define sc_age_lt4_lab  .m "No valid response", add
label define sc_age_lt4_lab  .n "Not in universe", add
label define sc_age_lt4_lab  .l "Logical skip", add
label define sc_age_lt4_lab  .d "Suppressed for confidentiality", add
cap label values sc_age_lt4 sc_age_lt4_lab
label define sc_age_lt6_lab  1  "LT 6 Months Old"
label define sc_age_lt6_lab  2  "GE 6 Months Old", add
label define sc_age_lt6_lab  .m "No valid response", add
label define sc_age_lt6_lab  .n "Not in universe", add
label define sc_age_lt6_lab  .l "Logical skip", add
label define sc_age_lt6_lab  .d "Suppressed for confidentiality", add
cap label values sc_age_lt6 sc_age_lt6_lab
label define sc_age_lt9_lab  1  "LT 9 Months Old"
label define sc_age_lt9_lab  2  "GE 9 Months Old", add
label define sc_age_lt9_lab  .m "No valid response", add
label define sc_age_lt9_lab  .n "Not in universe", add
label define sc_age_lt9_lab  .l "Logical skip", add
label define sc_age_lt9_lab  .d "Suppressed for confidentiality", add
cap label values sc_age_lt9 sc_age_lt9_lab
label define sc_age_years_lab  .m "No valid response"
label define sc_age_years_lab  .n "Not in universe", add
label define sc_age_years_lab  .l "Logical skip", add
label define sc_age_years_lab  .d "Suppressed for confidentiality", add
cap label values sc_age_years sc_age_years_lab
label define sc_aian_lab  1  "American Indian or Alaska Native"
label define sc_aian_lab  2  "Not American Indian or Alaska Native", add
label define sc_aian_lab  .m "No valid response", add
label define sc_aian_lab  .n "Not in universe", add
label define sc_aian_lab  .l "Logical skip", add
label define sc_aian_lab  .d "Suppressed for confidentiality", add
cap label values sc_aian sc_aian_lab
label define sc_asian_lab  1  "Asian"
label define sc_asian_lab  2  "Not Asian", add
label define sc_asian_lab  .m "No valid response", add
label define sc_asian_lab  .n "Not in universe", add
label define sc_asian_lab  .l "Logical skip", add
label define sc_asian_lab  .d "Suppressed for confidentiality", add
cap label values sc_asian sc_asian_lab
label define sc_cshcn_lab  1  "SHCN"
label define sc_cshcn_lab  2  "Non-SHCN", add
label define sc_cshcn_lab  .m "No valid response", add
label define sc_cshcn_lab  .n "Not in universe", add
label define sc_cshcn_lab  .l "Logical skip", add
label define sc_cshcn_lab  .d "Suppressed for confidentiality", add
cap label values sc_cshcn sc_cshcn_lab
label define sc_english_lab  1  "Very well"
label define sc_english_lab  2  "Well", add
label define sc_english_lab  3  "Not well", add
label define sc_english_lab  4  "Not at all", add
label define sc_english_lab  .m "No valid response", add
label define sc_english_lab  .n "Not in universe", add
label define sc_english_lab  .l "Logical skip", add
label define sc_english_lab  .d "Suppressed for confidentiality", add
cap label values sc_english sc_english_lab
label define sc_hispanic_r_lab  1  "Hispanic or Latino Origin"
label define sc_hispanic_r_lab  2  "Not Hispanic or Latino Origin", add
label define sc_hispanic_r_lab  .m "No valid response", add
label define sc_hispanic_r_lab  .n "Not in universe", add
label define sc_hispanic_r_lab  .l "Logical skip", add
label define sc_hispanic_r_lab  .d "Suppressed for confidentiality", add
cap label values sc_hispanic_r sc_hispanic_r_lab
label define sc_hispanic_r_if_lab  1  "Imputed"
label define sc_hispanic_r_if_lab  0  "Not imputed", add
label define sc_hispanic_r_if_lab  .m "No valid response", add
label define sc_hispanic_r_if_lab  .n "Not in universe", add
label define sc_hispanic_r_if_lab  .l "Logical skip", add
label define sc_hispanic_r_if_lab  .d "Suppressed for confidentiality", add
cap label values sc_hispanic_r_if sc_hispanic_r_if_lab
label define sc_k2q10_lab  1  "Yes"
label define sc_k2q10_lab  2  "No", add
label define sc_k2q10_lab  .m "No valid response", add
label define sc_k2q10_lab  .n "Not in universe", add
label define sc_k2q10_lab  .l "Logical skip", add
label define sc_k2q10_lab  .d "Suppressed for confidentiality", add
cap label values sc_k2q10 sc_k2q10_lab
label define sc_k2q11_lab  1  "Yes"
label define sc_k2q11_lab  2  "No", add
label define sc_k2q11_lab  .m "No valid response", add
label define sc_k2q11_lab  .n "Not in universe", add
label define sc_k2q11_lab  .l "Logical skip", add
label define sc_k2q11_lab  .d "Suppressed for confidentiality", add
cap label values sc_k2q11 sc_k2q11_lab
label define sc_k2q12_lab  1  "Yes"
label define sc_k2q12_lab  2  "No", add
label define sc_k2q12_lab  .m "No valid response", add
label define sc_k2q12_lab  .n "Not in universe", add
label define sc_k2q12_lab  .l "Logical skip", add
label define sc_k2q12_lab  .d "Suppressed for confidentiality", add
cap label values sc_k2q12 sc_k2q12_lab
label define sc_k2q13_lab  1  "Yes"
label define sc_k2q13_lab  2  "No", add
label define sc_k2q13_lab  .m "No valid response", add
label define sc_k2q13_lab  .n "Not in universe", add
label define sc_k2q13_lab  .l "Logical skip", add
label define sc_k2q13_lab  .d "Suppressed for confidentiality", add
cap label values sc_k2q13 sc_k2q13_lab
label define sc_k2q14_lab  1  "Yes"
label define sc_k2q14_lab  2  "No", add
label define sc_k2q14_lab  .m "No valid response", add
label define sc_k2q14_lab  .n "Not in universe", add
label define sc_k2q14_lab  .l "Logical skip", add
label define sc_k2q14_lab  .d "Suppressed for confidentiality", add
cap label values sc_k2q14 sc_k2q14_lab
label define sc_k2q15_lab  1  "Yes"
label define sc_k2q15_lab  2  "No", add
label define sc_k2q15_lab  .m "No valid response", add
label define sc_k2q15_lab  .n "Not in universe", add
label define sc_k2q15_lab  .l "Logical skip", add
label define sc_k2q15_lab  .d "Suppressed for confidentiality", add
cap label values sc_k2q15 sc_k2q15_lab
label define sc_k2q16_lab  1  "Yes"
label define sc_k2q16_lab  2  "No", add
label define sc_k2q16_lab  .m "No valid response", add
label define sc_k2q16_lab  .n "Not in universe", add
label define sc_k2q16_lab  .l "Logical skip", add
label define sc_k2q16_lab  .d "Suppressed for confidentiality", add
cap label values sc_k2q16 sc_k2q16_lab
label define sc_k2q17_lab  1  "Yes"
label define sc_k2q17_lab  2  "No", add
label define sc_k2q17_lab  .m "No valid response", add
label define sc_k2q17_lab  .n "Not in universe", add
label define sc_k2q17_lab  .l "Logical skip", add
label define sc_k2q17_lab  .d "Suppressed for confidentiality", add
cap label values sc_k2q17 sc_k2q17_lab
label define sc_k2q18_lab  1  "Yes"
label define sc_k2q18_lab  2  "No", add
label define sc_k2q18_lab  .m "No valid response", add
label define sc_k2q18_lab  .n "Not in universe", add
label define sc_k2q18_lab  .l "Logical skip", add
label define sc_k2q18_lab  .d "Suppressed for confidentiality", add
cap label values sc_k2q18 sc_k2q18_lab
label define sc_k2q19_lab  1  "Yes"
label define sc_k2q19_lab  2  "No", add
label define sc_k2q19_lab  .m "No valid response", add
label define sc_k2q19_lab  .n "Not in universe", add
label define sc_k2q19_lab  .l "Logical skip", add
label define sc_k2q19_lab  .d "Suppressed for confidentiality", add
cap label values sc_k2q19 sc_k2q19_lab
label define sc_k2q20_lab  1  "Yes"
label define sc_k2q20_lab  2  "No", add
label define sc_k2q20_lab  .m "No valid response", add
label define sc_k2q20_lab  .n "Not in universe", add
label define sc_k2q20_lab  .l "Logical skip", add
label define sc_k2q20_lab  .d "Suppressed for confidentiality", add
cap label values sc_k2q20 sc_k2q20_lab
label define sc_k2q21_lab  1  "Yes"
label define sc_k2q21_lab  2  "No", add
label define sc_k2q21_lab  .m "No valid response", add
label define sc_k2q21_lab  .n "Not in universe", add
label define sc_k2q21_lab  .l "Logical skip", add
label define sc_k2q21_lab  .d "Suppressed for confidentiality", add
cap label values sc_k2q21 sc_k2q21_lab
label define sc_k2q22_lab  1  "Yes"
label define sc_k2q22_lab  2  "No", add
label define sc_k2q22_lab  .m "No valid response", add
label define sc_k2q22_lab  .n "Not in universe", add
label define sc_k2q22_lab  .l "Logical skip", add
label define sc_k2q22_lab  .d "Suppressed for confidentiality", add
cap label values sc_k2q22 sc_k2q22_lab
label define sc_k2q23_lab  1  "Yes"
label define sc_k2q23_lab  2  "No", add
label define sc_k2q23_lab  .m "No valid response", add
label define sc_k2q23_lab  .n "Not in universe", add
label define sc_k2q23_lab  .l "Logical skip", add
label define sc_k2q23_lab  .d "Suppressed for confidentiality", add
cap label values sc_k2q23 sc_k2q23_lab
label define sc_nhpi_lab  1  "Native Hawaiian or Other Pacific Islanders"
label define sc_nhpi_lab  2  "Not Native Hawaiian or Other Pacific Islanders", add
label define sc_nhpi_lab  .m "No valid response", add
label define sc_nhpi_lab  .n "Not in universe", add
label define sc_nhpi_lab  .l "Logical skip", add
label define sc_nhpi_lab  .d "Suppressed for confidentiality", add
cap label values sc_nhpi sc_nhpi_lab
label define sc_race_r_lab  1  "White alone"
label define sc_race_r_lab  2  "Black or African American alone", add
label define sc_race_r_lab  3  "American Indian or Alaska Native alone", add
label define sc_race_r_lab  4  "Asian alone", add
label define sc_race_r_lab  5  "Native Hawaiian and Other Pacific Islander alone", add
label define sc_race_r_lab  7  "Two or More Races", add
label define sc_race_r_lab  .m "No valid response", add
label define sc_race_r_lab  .n "Not in universe", add
label define sc_race_r_lab  .l "Logical skip", add
label define sc_race_r_lab  .d "Suppressed for confidentiality", add
cap label values sc_race_r sc_race_r_lab
label define sc_race_r_if_lab  1  "Imputed"
label define sc_race_r_if_lab  0  "Not imputed", add
label define sc_race_r_if_lab  .m "No valid response", add
label define sc_race_r_if_lab  .n "Not in universe", add
label define sc_race_r_if_lab  .l "Logical skip", add
label define sc_race_r_if_lab  .d "Suppressed for confidentiality", add
cap label values sc_race_r_if sc_race_r_if_lab
label define sc_racer_lab  1  "White alone"
label define sc_racer_lab  2  "Black or African American alone", add
label define sc_racer_lab  3  "Other", add
label define sc_racer_lab  .m "No valid response", add
label define sc_racer_lab  .n "Not in universe", add
label define sc_racer_lab  .l "Logical skip", add
label define sc_racer_lab  .d "Suppressed for confidentiality", add
cap label values sc_racer sc_racer_lab
label define sc_sex_lab  1  "Male"
label define sc_sex_lab  2  "Female", add
label define sc_sex_lab  .m "No valid response", add
label define sc_sex_lab  .n "Not in universe", add
label define sc_sex_lab  .l "Logical skip", add
label define sc_sex_lab  .d "Suppressed for confidentiality", add
cap label values sc_sex sc_sex_lab
label define sc_sex_if_lab  1  "Imputed"
label define sc_sex_if_lab  0  "Not imputed", add
label define sc_sex_if_lab  .m "No valid response", add
label define sc_sex_if_lab  .n "Not in universe", add
label define sc_sex_if_lab  .l "Logical skip", add
label define sc_sex_if_lab  .d "Suppressed for confidentiality", add
cap label values sc_sex_if sc_sex_if_lab
label define screentime_lab  1  "Less than 1 hour"
label define screentime_lab  2  "1 hour", add
label define screentime_lab  3  "2 hours", add
label define screentime_lab  4  "3 hours", add
label define screentime_lab  5  "4 or more hours", add
label define screentime_lab  .m "No valid response", add
label define screentime_lab  .n "Not in universe", add
label define screentime_lab  .l "Logical skip", add
label define screentime_lab  .d "Suppressed for confidentiality", add
cap label values screentime screentime_lab
label define seekcare_lab  1  "Yes"
label define seekcare_lab  2  "No", add
label define seekcare_lab  .m "No valid response", add
label define seekcare_lab  .n "Not in universe", add
label define seekcare_lab  .l "Logical skip", add
label define seekcare_lab  .d "Suppressed for confidentiality", add
cap label values seekcare seekcare_lab
label define sescurrsvc_lab  1  "Yes"
label define sescurrsvc_lab  2  "No", add
label define sescurrsvc_lab  .m "No valid response", add
label define sescurrsvc_lab  .n "Not in universe", add
label define sescurrsvc_lab  .l "Logical skip", add
label define sescurrsvc_lab  .d "Suppressed for confidentiality", add
cap label values sescurrsvc sescurrsvc_lab
label define sesplanmo_lab  .m "No valid response"
label define sesplanmo_lab  .n "Not in universe", add
label define sesplanmo_lab  .l "Logical skip", add
label define sesplanmo_lab  .d "Suppressed for confidentiality", add
cap label values sesplanmo sesplanmo_lab
label define sesplanyr_lab  .m "No valid response"
label define sesplanyr_lab  .n "Not in universe", add
label define sesplanyr_lab  .l "Logical skip", add
label define sesplanyr_lab  .d "Suppressed for confidentiality", add
cap label values sesplanyr sesplanyr_lab
label define sharetoys_lab  1  "Always"
label define sharetoys_lab  2  "Most of the time", add
label define sharetoys_lab  3  "About half the time", add
label define sharetoys_lab  4  "Sometimes", add
label define sharetoys_lab  5  "Never", add
label define sharetoys_lab  .m "No valid response", add
label define sharetoys_lab  .n "Not in universe", add
label define sharetoys_lab  .l "Logical skip", add
label define sharetoys_lab  .d "Suppressed for confidentiality", add
cap label values sharetoys sharetoys_lab
label define sicklecell_lab  1  "Yes"
label define sicklecell_lab  2  "No", add
label define sicklecell_lab  .m "No valid response", add
label define sicklecell_lab  .n "Not in universe", add
label define sicklecell_lab  .l "Logical skip", add
label define sicklecell_lab  .d "Suppressed for confidentiality", add
cap label values sicklecell sicklecell_lab
label define simpleaddition_lab  1  "Always"
label define simpleaddition_lab  2  "Most of the time", add
label define simpleaddition_lab  3  "About half the time", add
label define simpleaddition_lab  4  "Sometimes", add
label define simpleaddition_lab  5  "Never", add
label define simpleaddition_lab  .m "No valid response", add
label define simpleaddition_lab  .n "Not in universe", add
label define simpleaddition_lab  .l "Logical skip", add
label define simpleaddition_lab  .d "Suppressed for confidentiality", add
cap label values simpleaddition simpleaddition_lab
label define sleeppos_lab  1  "On their side"
label define sleeppos_lab  2  "On their back", add
label define sleeppos_lab  3  "On their stomach", add
label define sleeppos_lab  .m "No valid response", add
label define sleeppos_lab  .n "Not in universe", add
label define sleeppos_lab  .l "Logical skip", add
label define sleeppos_lab  .d "Suppressed for confidentiality", add
cap label values sleeppos sleeppos_lab
label define spcservmo_lab  .m "No valid response"
label define spcservmo_lab  .n "Not in universe", add
label define spcservmo_lab  .l "Logical skip", add
label define spcservmo_lab  .d "Suppressed for confidentiality", add
cap label values spcservmo spcservmo_lab
label define ssi_lab  1  "Yes"
label define ssi_lab  2  "No", add
label define ssi_lab  .m "No valid response", add
label define ssi_lab  .n "Not in universe", add
label define ssi_lab  .l "Logical skip", add
label define ssi_lab  .d "Suppressed for confidentiality", add
cap label values ssi ssi_lab
label define ssidisability_lab  1  "Yes"
label define ssidisability_lab  2  "No", add
label define ssidisability_lab  .m "No valid response", add
label define ssidisability_lab  .n "Not in universe", add
label define ssidisability_lab  .l "Logical skip", add
label define ssidisability_lab  .d "Suppressed for confidentiality", add
cap label values ssidisability ssidisability_lab
label define startnewact_lab  1  "Always"
label define startnewact_lab  2  "Most of the time", add
label define startnewact_lab  3  "About half the time", add
label define startnewact_lab  4  "Sometimes", add
label define startnewact_lab  5  "Never", add
label define startnewact_lab  .m "No valid response", add
label define startnewact_lab  .n "Not in universe", add
label define startnewact_lab  .l "Logical skip", add
label define startnewact_lab  .d "Suppressed for confidentiality", add
cap label values startnewact startnewact_lab
label define startschool_lab  1  "Yes, preschool"
label define startschool_lab  2  "Yes, kindergarten", add
label define startschool_lab  3  "Yes, first grade", add
label define startschool_lab  4  "No", add
label define startschool_lab  .m "No valid response", add
label define startschool_lab  .n "Not in universe", add
label define startschool_lab  .l "Logical skip", add
label define startschool_lab  .d "Suppressed for confidentiality", add
cap label values startschool startschool_lab
label define stomach_lab  1  "Yes"
label define stomach_lab  2  "No", add
label define stomach_lab  .m "No valid response", add
label define stomach_lab  .n "Not in universe", add
label define stomach_lab  .l "Logical skip", add
label define stomach_lab  .d "Suppressed for confidentiality", add
cap label values stomach stomach_lab
label define stopwork_lab  1  "Yes"
label define stopwork_lab  2  "No", add
label define stopwork_lab  .m "No valid response", add
label define stopwork_lab  .n "Not in universe", add
label define stopwork_lab  .l "Logical skip", add
label define stopwork_lab  .d "Suppressed for confidentiality", add
cap label values stopwork stopwork_lab
label define strengths_lab  1  "All of the time"
label define strengths_lab  2  "Most of the time", add
label define strengths_lab  3  "Some of the time", add
label define strengths_lab  4  "None of the time", add
label define strengths_lab  .m "No valid response", add
label define strengths_lab  .n "Not in universe", add
label define strengths_lab  .l "Logical skip", add
label define strengths_lab  .d "Suppressed for confidentiality", add
cap label values strengths strengths_lab
label define sugardrink_lab  1  "This child did not drink sugary drinks"
label define sugardrink_lab  2  "1-3 times during the past week", add
label define sugardrink_lab  3  "4-6 times during the past week", add
label define sugardrink_lab  4  "1 time per day", add
label define sugardrink_lab  5  "2 times per day", add
label define sugardrink_lab  6  "3 or more times per day", add
label define sugardrink_lab  .m "No valid response", add
label define sugardrink_lab  .n "Not in universe", add
label define sugardrink_lab  .l "Logical skip", add
label define sugardrink_lab  .d "Suppressed for confidentiality", add
cap label values sugardrink sugardrink_lab
label define swallowing_lab  1  "Yes"
label define swallowing_lab  2  "No", add
label define swallowing_lab  .m "No valid response", add
label define swallowing_lab  .n "Not in universe", add
label define swallowing_lab  .l "Logical skip", add
label define swallowing_lab  .d "Suppressed for confidentiality", add
cap label values swallowing swallowing_lab
label define talkabout_lab  1  "All of the time"
label define talkabout_lab  2  "Most of the time", add
label define talkabout_lab  3  "Some of the time", add
label define talkabout_lab  4  "None of the time", add
label define talkabout_lab  .m "No valid response", add
label define talkabout_lab  .n "Not in universe", add
label define talkabout_lab  .l "Logical skip", add
label define talkabout_lab  .d "Suppressed for confidentiality", add
cap label values talkabout talkabout_lab
label define tellstory_lab  1  "Yes"
label define tellstory_lab  2  "No", add
label define tellstory_lab  .m "No valid response", add
label define tellstory_lab  .n "Not in universe", add
label define tellstory_lab  .l "Logical skip", add
label define tellstory_lab  .d "Suppressed for confidentiality", add
cap label values tellstory tellstory_lab
label define temper_r_lab  1  "Always"
label define temper_r_lab  2  "Most of the time", add
label define temper_r_lab  3  "About half the time", add
label define temper_r_lab  4  "Sometimes", add
label define temper_r_lab  5  "Never", add
label define temper_r_lab  .m "No valid response", add
label define temper_r_lab  .n "Not in universe", add
label define temper_r_lab  .l "Logical skip", add
label define temper_r_lab  .d "Suppressed for confidentiality", add
cap label values temper_r temper_r_lab
label define tenure_lab  1  "Owned by you or someone in this household with a mortgage or loan? Include home equity loans"
label define tenure_lab  2  "Owned by you or someone in this household free and clear (without a mortgage or loan)?", add
label define tenure_lab  3  "Rented?", add
label define tenure_lab  4  "Occupied without payment of rent?", add
label define tenure_lab  .m "No valid response", add
label define tenure_lab  .n "Not in universe", add
label define tenure_lab  .l "Logical skip", add
label define tenure_lab  .d "Suppressed for confidentiality", add
cap label values tenure tenure_lab
label define tenure_if_lab  1  "Imputed"
label define tenure_if_lab  0  "Not imputed", add
label define tenure_if_lab  .m "No valid response", add
label define tenure_if_lab  .n "Not in universe", add
label define tenure_if_lab  .l "Logical skip", add
label define tenure_if_lab  .d "Suppressed for confidentiality", add
cap label values tenure_if tenure_if_lab
label define thalassemia_lab  1  "Yes"
label define thalassemia_lab  2  "No", add
label define thalassemia_lab  .m "No valid response", add
label define thalassemia_lab  .n "Not in universe", add
label define thalassemia_lab  .l "Logical skip", add
label define thalassemia_lab  .d "Suppressed for confidentiality", add
cap label values thalassemia thalassemia_lab
label define threewords_lab  1  "Yes"
label define threewords_lab  2  "No", add
label define threewords_lab  .m "No valid response", add
label define threewords_lab  .n "Not in universe", add
label define threewords_lab  .l "Logical skip", add
label define threewords_lab  .d "Suppressed for confidentiality", add
cap label values threewords threewords_lab
label define toothaches_lab  1  "Yes"
label define toothaches_lab  2  "No", add
label define toothaches_lab  .m "No valid response", add
label define toothaches_lab  .n "Not in universe", add
label define toothaches_lab  .l "Logical skip", add
label define toothaches_lab  .d "Suppressed for confidentiality", add
cap label values toothaches toothaches_lab
label define totage_0_5_lab  .m "No valid response"
label define totage_0_5_lab  .n "Not in universe", add
label define totage_0_5_lab  .l "Logical skip", add
label define totage_0_5_lab  .d "Suppressed for confidentiality", add
cap label values totage_0_5 totage_0_5_lab
label define totage_12_17_lab  .m "No valid response"
label define totage_12_17_lab  .n "Not in universe", add
label define totage_12_17_lab  .l "Logical skip", add
label define totage_12_17_lab  .d "Suppressed for confidentiality", add
cap label values totage_12_17 totage_12_17_lab
label define totage_6_11_lab  .m "No valid response"
label define totage_6_11_lab  .n "Not in universe", add
label define totage_6_11_lab  .l "Logical skip", add
label define totage_6_11_lab  .d "Suppressed for confidentiality", add
cap label values totage_6_11 totage_6_11_lab
label define totcshcn_lab  .m "No valid response"
label define totcshcn_lab  .n "Not in universe", add
label define totcshcn_lab  .l "Logical skip", add
label define totcshcn_lab  .d "Suppressed for confidentiality", add
cap label values totcshcn totcshcn_lab
label define totfemale_lab  .m "No valid response"
label define totfemale_lab  .n "Not in universe", add
label define totfemale_lab  .l "Logical skip", add
label define totfemale_lab  .d "Suppressed for confidentiality", add
cap label values totfemale totfemale_lab
label define totkids_r_lab  1  "1"
label define totkids_r_lab  2  "2", add
label define totkids_r_lab  3  "3", add
label define totkids_r_lab  4  "4+", add
label define totkids_r_lab  .m "No valid response", add
label define totkids_r_lab  .n "Not in universe", add
label define totkids_r_lab  .l "Logical skip", add
label define totkids_r_lab  .d "Suppressed for confidentiality", add
cap label values totkids_r totkids_r_lab
label define totmale_lab  .m "No valid response"
label define totmale_lab  .n "Not in universe", add
label define totmale_lab  .l "Logical skip", add
label define totmale_lab  .d "Suppressed for confidentiality", add
cap label values totmale totmale_lab
label define totnonshcn_lab  .m "No valid response"
label define totnonshcn_lab  .n "Not in universe", add
label define totnonshcn_lab  .l "Logical skip", add
label define totnonshcn_lab  .d "Suppressed for confidentiality", add
cap label values totnonshcn totnonshcn_lab
label define transportcc_lab  1  "Yes"
label define transportcc_lab  2  "No", add
label define transportcc_lab  .m "No valid response", add
label define transportcc_lab  .n "Not in universe", add
label define transportcc_lab  .l "Logical skip", add
label define transportcc_lab  .d "Suppressed for confidentiality", add
cap label values transportcc transportcc_lab
label define treatadult_lab  1  "Yes"
label define treatadult_lab  2  "No", add
label define treatadult_lab  .m "No valid response", add
label define treatadult_lab  .n "Not in universe", add
label define treatadult_lab  .l "Logical skip", add
label define treatadult_lab  .d "Suppressed for confidentiality", add
cap label values treatadult treatadult_lab
label define treatchild_lab  1  "Yes"
label define treatchild_lab  2  "No", add
label define treatchild_lab  .m "No valid response", add
label define treatchild_lab  .n "Not in universe", add
label define treatchild_lab  .l "Logical skip", add
label define treatchild_lab  .d "Suppressed for confidentiality", add
cap label values treatchild treatchild_lab
label define treatneed_lab  1  "Not difficult"
label define treatneed_lab  2  "Somewhat difficult", add
label define treatneed_lab  3  "Very difficult", add
label define treatneed_lab  4  "It was not possible to obtain care", add
label define treatneed_lab  .m "No valid response", add
label define treatneed_lab  .n "Not in universe", add
label define treatneed_lab  .l "Logical skip", add
label define treatneed_lab  .d "Suppressed for confidentiality", add
cap label values treatneed treatneed_lab
label define tricare_lab  1  "Yes"
label define tricare_lab  2  "No", add
label define tricare_lab  .m "No valid response", add
label define tricare_lab  .n "Not in universe", add
label define tricare_lab  .l "Logical skip", add
label define tricare_lab  .d "Suppressed for confidentiality", add
cap label values tricare tricare_lab
label define twowords_lab  1  "Yes"
label define twowords_lab  2  "No", add
label define twowords_lab  .m "No valid response", add
label define twowords_lab  .n "Not in universe", add
label define twowords_lab  .l "Logical skip", add
label define twowords_lab  .d "Suppressed for confidentiality", add
cap label values twowords twowords_lab
label define understand_lab  1  "Yes"
label define understand_lab  2  "No", add
label define understand_lab  .m "No valid response", add
label define understand_lab  .n "Not in universe", add
label define understand_lab  .l "Logical skip", add
label define understand_lab  .d "Suppressed for confidentiality", add
cap label values understand understand_lab
label define understand2_lab  1  "Yes"
label define understand2_lab  2  "No", add
label define understand2_lab  .m "No valid response", add
label define understand2_lab  .n "Not in universe", add
label define understand2_lab  .l "Logical skip", add
label define understand2_lab  .d "Suppressed for confidentiality", add
cap label values understand2 understand2_lab
label define usualgo_lab  1  "Yes"
label define usualgo_lab  2  "No", add
label define usualgo_lab  .m "No valid response", add
label define usualgo_lab  .n "Not in universe", add
label define usualgo_lab  .l "Logical skip", add
label define usualgo_lab  .d "Suppressed for confidentiality", add
cap label values usualgo usualgo_lab
label define usualsick_lab  1  "Yes"
label define usualsick_lab  2  "No", add
label define usualsick_lab  .m "No valid response", add
label define usualsick_lab  .n "Not in universe", add
label define usualsick_lab  .l "Logical skip", add
label define usualsick_lab  .d "Suppressed for confidentiality", add
cap label values usualsick usualsick_lab
label define vape_lab  1  "Yes"
label define vape_lab  2  "No", add
label define vape_lab  .m "No valid response", add
label define vape_lab  .n "Not in universe", add
label define vape_lab  .l "Logical skip", add
label define vape_lab  .d "Suppressed for confidentiality", add
cap label values vape vape_lab
label define vegetables_lab  1  "This child did not eat vegetables"
label define vegetables_lab  2  "1-3 times during the past week", add
label define vegetables_lab  3  "4-6 times during the past week", add
label define vegetables_lab  4  "1 time per day", add
label define vegetables_lab  5  "2 times per day", add
label define vegetables_lab  6  "3 or more times per day", add
label define vegetables_lab  .m "No valid response", add
label define vegetables_lab  .n "Not in universe", add
label define vegetables_lab  .l "Logical skip", add
label define vegetables_lab  .d "Suppressed for confidentiality", add
cap label values vegetables vegetables_lab
label define videophone_lab  1  "Yes"
label define videophone_lab  2  "No", add
label define videophone_lab  .m "No valid response", add
label define videophone_lab  .n "Not in universe", add
label define videophone_lab  .l "Logical skip", add
label define videophone_lab  .d "Suppressed for confidentiality", add
cap label values videophone videophone_lab
label define videophonecovid_lab  1  "Yes"
label define videophonecovid_lab  2  "No", add
label define videophonecovid_lab  .m "No valid response", add
label define videophonecovid_lab  .n "Not in universe", add
label define videophonecovid_lab  .l "Logical skip", add
label define videophonecovid_lab  .d "Suppressed for confidentiality", add
cap label values videophonecovid videophonecovid_lab
label define visionexamrec_lab  1  "Yes"
label define visionexamrec_lab  2  "No", add
label define visionexamrec_lab  .m "No valid response", add
label define visionexamrec_lab  .n "Not in universe", add
label define visionexamrec_lab  .l "Logical skip", add
label define visionexamrec_lab  .d "Suppressed for confidentiality", add
cap label values visionexamrec visionexamrec_lab
label define visionscreenother_lab  1  "Yes"
label define visionscreenother_lab  2  "No", add
label define visionscreenother_lab  .m "No valid response", add
label define visionscreenother_lab  .n "Not in universe", add
label define visionscreenother_lab  .l "Logical skip", add
label define visionscreenother_lab  .d "Suppressed for confidentiality", add
cap label values visionscreenother visionscreenother_lab
label define waitforturn_lab  1  "Always"
label define waitforturn_lab  2  "Most of the time", add
label define waitforturn_lab  3  "About half the time", add
label define waitforturn_lab  4  "Sometimes", add
label define waitforturn_lab  5  "Never", add
label define waitforturn_lab  .m "No valid response", add
label define waitforturn_lab  .n "Not in universe", add
label define waitforturn_lab  .l "Logical skip", add
label define waitforturn_lab  .d "Suppressed for confidentiality", add
cap label values waitforturn waitforturn_lab
label define walkstairs_lab  1  "Yes"
label define walkstairs_lab  2  "No", add
label define walkstairs_lab  .m "No valid response", add
label define walkstairs_lab  .n "Not in universe", add
label define walkstairs_lab  .l "Logical skip", add
label define walkstairs_lab  .d "Suppressed for confidentiality", add
cap label values walkstairs walkstairs_lab
label define wgtconc_lab  1  "Yes, it's too high"
label define wgtconc_lab  2  "Yes, it's too low", add
label define wgtconc_lab  3  "No, not concerned", add
label define wgtconc_lab  .m "No valid response", add
label define wgtconc_lab  .n "Not in universe", add
label define wgtconc_lab  .l "Logical skip", add
label define wgtconc_lab  .d "Suppressed for confidentiality", add
cap label values wgtconc wgtconc_lab
label define wktosolve_lab  1  "All of the time"
label define wktosolve_lab  2  "Most of the time", add
label define wktosolve_lab  3  "Some of the time", add
label define wktosolve_lab  4  "None of the time", add
label define wktosolve_lab  .m "No valid response", add
label define wktosolve_lab  .n "Not in universe", add
label define wktosolve_lab  .l "Logical skip", add
label define wktosolve_lab  .d "Suppressed for confidentiality", add
cap label values wktosolve wktosolve_lab
label define writename_lab  1  "Always"
label define writename_lab  2  "Most of the time", add
label define writename_lab  3  "About half the time", add
label define writename_lab  4  "Sometimes", add
label define writename_lab  5  "Never", add
label define writename_lab  .m "No valid response", add
label define writename_lab  .n "Not in universe", add
label define writename_lab  .l "Logical skip", add
label define writename_lab  .d "Suppressed for confidentiality", add
cap label values writename writename_lab
label define writeplan_lab  1  "Yes"
label define writeplan_lab  2  "No", add
label define writeplan_lab  .m "No valid response", add
label define writeplan_lab  .n "Not in universe", add
label define writeplan_lab  .l "Logical skip", add
label define writeplan_lab  .d "Suppressed for confidentiality", add
cap label values writeplan writeplan_lab
label define year_lab  .m "No valid response"
label define year_lab  .n "Not in universe", add
label define year_lab  .l "Logical skip", add
label define year_lab  .d "Suppressed for confidentiality", add
cap label values year year_lab
label define height_lab  .m "No valid response"
label define height_lab  .n "Not in universe", add
label define height_lab  .l "Logical skip", add
label define height_lab  .d "Suppressed for confidentiality", add
cap label values height height_lab
label define weight_lab  .m "No valid response"
label define weight_lab  .n "Not in universe", add
label define weight_lab  .l "Logical skip", add
label define weight_lab  .d "Suppressed for confidentiality", add
cap label values weight weight_lab

label define a1_age_lab 75 "75 or older", add
label define a1_liveusa_lab 1970 "1970 or earlier", add
label define a2_age_lab 75 "75 or older", add
label define a2_liveusa_lab 1970 "1970 or earlier", add
label define birthwt_oz_s_lab 155 "155 or more", add
label define birthwt_oz_s_lab 72 "72 or less", add
label define breastfedend_mo_s_lab 29 "29 or more", add
label define famcount_lab 8 "8 or more", add
label define frstformula_mo_s_lab 12 "12 or more", add
label define frstsolids_mo_s_lab 15 "15 or more", add
label define hhcount_lab 10 "10 or more", add
label define k11q43r_lab 15 "15 or more", add
label define k2q35a_1_years_lab 15 "15 to 17", add
label define k2q35a_1_years_lab 1 "0 or 1", add
label define k4q37_lab 16 "16 or 17", add
label define liveusa_mo_lab 1 "0, 1, or 2", add
label define liveusa_mo_lab 10 "9, 10, or 11", add
label define liveusa_mo_lab 4 "3, 4, or 5", add
label define liveusa_mo_lab 7 "6, 7, or 8", add
label define momage_lab 18 "18 or younger", add
label define momage_lab 45 "45 or older", add

label define fpl_i1_lab 50 "50 or less", add
label define fpl_i1_lab 400 "400 or more", add
label define fpl_i2_lab 50 "50 or less", add
label define fpl_i2_lab 400 "400 or more", add
label define fpl_i3_lab 50 "50 or less", add
label define fpl_i3_lab 400 "400 or more", add
label define fpl_i4_lab 50 "50 or less", add
label define fpl_i4_lab 400 "400 or more", add
label define fpl_i5_lab 50 "50 or less", add
label define fpl_i5_lab 400 "400 or more", add
label define fpl_i6_lab 50 "50 or less", add
label define fpl_i6_lab 400 "400 or more", add

cap label values a1_age a1_age_lab
cap label values a1_liveusa a1_liveusa_lab
cap label values a2_age a2_age_lab
cap label values a2_liveusa a2_liveusa_lab
cap label values birthwt_oz_s birthwt_oz_s_lab
cap label values breastfedend_mo_s breastfedend_mo_s_lab
cap label values famcount famcount_lab
cap label values frstformula_mo_s frstformula_mo_s_lab
cap label values frstsolids_mo_s frstsolids_mo_s_lab
cap label values hhcount hhcount_lab
cap label values k11q43r k11q43r_lab
cap label values k2q35a_1_years k2q35a_1_years_lab
cap label values k4q37 k4q37_lab
cap label values liveusa_mo liveusa_mo_lab
cap label values momage momage_lab
cap label values fpl_i1 fpl_i1_lab
cap label values fpl_i2 fpl_i2_lab
cap label values fpl_i3 fpl_i3_lab
cap label values fpl_i4 fpl_i4_lab
cap label values fpl_i5 fpl_i5_lab
cap label values fpl_i6 fpl_i6_lab

label define fipsst_lab 01 "Alabama", add
label define fipsst_lab 02 "Alaska", add
label define fipsst_lab 04 "Arizona", add
label define fipsst_lab 05 "Arkansas", add
label define fipsst_lab 06 "California", add
label define fipsst_lab 08 "Colorado", add
label define fipsst_lab 09 "Connecticut", add
label define fipsst_lab 10 "Delaware", add
label define fipsst_lab 11 "District of Columbia", add
label define fipsst_lab 12 "Florida", add
label define fipsst_lab 13 "Georgia", add
label define fipsst_lab 15 "Hawaii", add
label define fipsst_lab 16 "Idaho", add
label define fipsst_lab 17 "Illinois", add
label define fipsst_lab 18 "Indiana", add
label define fipsst_lab 19 "Iowa", add
label define fipsst_lab 20 "Kansas", add
label define fipsst_lab 21 "Kentucky", add
label define fipsst_lab 22 "Louisiana", add
label define fipsst_lab 23 "Maine", add
label define fipsst_lab 24 "Maryland", add
label define fipsst_lab 25 "Massachusetts", add
label define fipsst_lab 26 "Michigan", add
label define fipsst_lab 27 "Minnesota", add
label define fipsst_lab 28 "Mississippi", add
label define fipsst_lab 29 "Missouri", add
label define fipsst_lab 30 "Montana", add
label define fipsst_lab 31 "Nebraska", add
label define fipsst_lab 32 "Nevada", add
label define fipsst_lab 33 "New Hampshire", add
label define fipsst_lab 34 "New Jersey", add
label define fipsst_lab 35 "New Mexico", add
label define fipsst_lab 36 "New York", add
label define fipsst_lab 37 "North Carolina", add
label define fipsst_lab 38 "North Dakota", add
label define fipsst_lab 39 "Ohio", add
label define fipsst_lab 40 "Oklahoma", add
label define fipsst_lab 41 "Oregon", add
label define fipsst_lab 42 "Pennsylvania", add
label define fipsst_lab 44 "Rhode Island", add
label define fipsst_lab 45 "South Carolina", add
label define fipsst_lab 46 "South Dakota", add
label define fipsst_lab 47 "Tennessee", add
label define fipsst_lab 48 "Texas", add
label define fipsst_lab 49 "Utah", add
label define fipsst_lab 50 "Vermont", add
label define fipsst_lab 51 "Virginia", add
label define fipsst_lab 53 "Washington", add
label define fipsst_lab 54 "West Virginia", add
label define fipsst_lab 55 "Wisconsin", add
label define fipsst_lab 56 "Wyoming", add
cap label values fipsst fipsst_lab
}

quietly {
/* SET UP SURVEY SETTINGS */

/* using variable names from the original NSCH dataset: */
egen statacross=group(fipsst stratum)
svyset hhid, strata(statacross) weight(fwc)

/* GENERATE NECESSARY VARIABLES */

/* translate some NSCH variable names to readable variable names: */
gen weight_op=fwc
gen sex_mf=sc_sex
gen race_detailed=sc_race_r
gen ever_diag_asd=k2q35a
gen ever_diag_adhd=k2q31a
gen insurancetype=instype
gen age_years=sc_age_years
egen fpl = rmean(fpl_i1-fpl_i6)
gen hispanic_r=sc_hispanic_r
gen family_numchild=totkids_r

/* translate the variables to other useful variables and label them */
gen int_weight_op = int(weight_op)

recode sex_mf (1=0) (2=1), generate(female)
label define female 0 "male" 1 "female" 
label values female female


label define race_detailed 1 "white" 2 "black" 3 "AIAN" 4 "asian" 5 "NHPI" 7 "2+" 
label values race_detailed race_detailed


recode ever_diag_asd (1=1) (2=0), generate(asd)
label define asd 0 "non asd" 1 "asd" 
label values asd asd

recode ever_diag_adhd (1=1) (2=0), generate(adhd)
label define adhd 0 "non adhd" 1 "adhd"  
label values adhd adhd

gen racethfull = (2-hispanic_r)*7 + race_detailed
recode racethfull (1=1) (2=2) (3=3) (4=4) (5=5) (7=7) (8=8) (9=8) (10=8) (11=8) (12=8) (14=8), generate(raceth)

label define racethfull 1 "nh_white" 2 "nh_black" 3 "nh_AIAN" 4 "nh_asian" 5 "nh_NHPI" 7 "nh_2+" 8 "h_white" 9 "h_black" 10 "h_AIAN" 11 "h_asian" 12 "h_NHPI" 14 "h_2+"
label values racethfull racethfull

label define raceth 1 "nh_white" 2 "nh_black" 3 "nh_AIAN" 4 "nh_asian" 5 "nh_NHPI" 7 "nh_2+" 8 "hispanic"
label values raceth raceth

label define insurancetype 1 "public" 2 "private" 3 "both" 5 "neither"
label values insurancetype insurancetype

label define family_numchild 1 "1 child" 2 "2 children" 3 "3 children" 4 "4+ children"
label values family_numchild family_numchild

egen age_cat = cut(age_years), at(0,2,5,10,13,18) icodes /*0,0-1:1,2-4:2,5-9:3,10-12:4,13-*/
label define age_cat 0 "0-1" 1 "2-4" 2 "5-9" 3 "10-12" 4 "13+"
label values age_cat age_cat


/* CREATE CATEGORICAL FPL */ 
egen fpl_cat = cut(fpl), at(49,100,150,200,250,300,350,401) icodes
label define fpl_cat 0 "0-99" 1 "100-149" 2 "150-199" 3 "200-249" 4 "250-299" 5 "300-349" 6 "350-399" 7 "400+"
label values fpl_cat fpl_cat
}


/* TABLE 1 */

svy: tabulate female
svy: tabulate female asd, row
svy: tabulate female adhd, row

svy, subpop(if female==1): tabulate fpl_cat
svy, subpop(if female==1): tabulate fpl_cat asd, row
svy, subpop(if female==1): tabulate fpl_cat adhd, row

svy, subpop(if female==1): tabulate age_cat 
svy, subpop(if female==1): tabulate age_cat asd, row /* non significant */
svy, subpop(if female==1): tabulate age_cat adhd, row

svy, subpop(if female==1): tabulate racethfull
svy, subpop(if female==1): tabulate racethfull asd, row /* non significant */
svy, subpop(if female==0): tabulate racethfull asd, row
svy, subpop(if female==1): tabulate racethfull adhd, row
svy, subpop(if female==0): tabulate racethfull adhd, row

svy, subpop(if female==1): tabulate age_years
svy, subpop(if female==1): tabulate age_years asd, row /* non significant */
svy, subpop(if female==0): tabulate age_years asd, row
svy, subpop(if female==1): tabulate age_years adhd, row

svy, subpop(if female==1): tabulate raceth
svy, subpop(if female==1): tabulate raceth asd, row /* non significant */
svy, subpop(if female==1): tabulate raceth adhd, row

svy, subpop(if female==1): tabulate insurancetype
svy, subpop(if female==1): tabulate insurancetype asd, row
svy, subpop(if female==1): tabulate insurancetype adhd, row

svy, subpop(if female==1): tabulate family_numchild
svy, subpop(if female==1): tabulate family_numchild asd, row /* non significant */
svy, subpop(if female==1): tabulate family_numchild adhd, row



/* TABLE 2 */

/* TEST LOGISTIC REGS TO HAVE BASELINE OddsRatio FOR FPL */
svy, subpop(if female==1): logistic asd fpl
estimates store s

svy, subpop(if female==1): logistic adhd fpl
estimates store h

quietly {
svy, subpop(if female==1): logistic asd fpl c.fpl#c.fpl
estimates store ss

svy, subpop(if female==1): logistic adhd fpl c.fpl#c.fpl
estimates store hs
}


/* LOGISTIC REG MODEL WITHOUT QUADRATIC FPL TERM */


/* ASD model 1 */
svy, subpop(if female==1): logistic asd fpl age_years i.racethfull i.insurancetype family_numchild, coeflegend
estimates store ms

/* ADHD model 2 */
svy, subpop(if female==1): logistic adhd fpl age_years i.racethfull i.insurancetype family_numchild, coeflegend
estimates store mh



/* LOGISTIC REG MODEL WITH QUADRATIC FPL TERM */


/* ASD model 3 */
svy, subpop(if female==1): logistic asd fpl c.fpl#c.fpl age_years i.racethfull i.insurancetype family_numchild, coeflegend
estimates store mss
if _b[c.fpl#c.fpl] > 0 display "trough" 
else display "peak"
nlcom -_b[fpl]/(2*_b[c.fpl#c.fpl])


/* ADHD model 4 */
svy, subpop(if female==1): logistic adhd fpl c.fpl#c.fpl age_years i.racethfull i.insurancetype family_numchild, coeflegend
estimates store mhs
if _b[c.fpl#c.fpl] > 0 display "trough" 
else display "peak"
nlcom -_b[fpl]/(2*_b[c.fpl#c.fpl])


/* LOGISTIC REG MODEL WITH QUADRATIC FPL TERM BOYS */

/* ASD */
quietly: svy, subpop(if female==0): logistic asd fpl c.fpl#c.fpl age_years i.racethfull i.insurancetype family_numchild, coeflegend
if _b[c.fpl#c.fpl] > 0 display "trough" 
else display "peak"
nlcom -_b[fpl]/(2*_b[c.fpl#c.fpl])

/* ADHD */
quietly: svy, subpop(if female==0): logistic adhd fpl c.fpl#c.fpl age_years i.racethfull i.insurancetype family_numchild, coeflegend
if _b[c.fpl#c.fpl] > 0 display "trough" 
else display "peak"
nlcom -_b[fpl]/(2*_b[c.fpl#c.fpl])

/* SUEST */

suest ms mh mss mhs, eform(Odds ratio)
quietly: suest, coeflegend


/* POSTTESTs */


/* is logistic regression fpl OR the same between non quadratic models? */
test _b[ms_asd:fpl] = _b[mh_adhd:fpl]
local sign_wgt = sign(_b[mh_adhd:fpl]-_b[ms_asd:fpl])
display "Ho: mod1 <= mod2  p-value = " 1-ttail(r(df_r),`sign_wgt'*sqrt(r(F)))
display "Ho: mod1 >= mod2  p-value = " ttail(r(df_r),`sign_wgt'*sqrt(r(F)))


/* is logistic regression fpl OR the same between quadratic models? */
test _b[mss_asd:fpl] = _b[mhs_adhd:fpl]
local sign_wgt = sign(_b[mhs_adhd:fpl]-_b[mss_asd:fpl])
display "Ho: mod3 <= mod4  p-value = " 1-ttail(r(df_r),`sign_wgt'*sqrt(r(F)))
display "Ho: mod3 >= mod4  p-value = " ttail(r(df_r),`sign_wgt'*sqrt(r(F)))


/* is logistic regression fpl^2 OR the same between quadratic models? */
test _b[mss_asd:c.fpl#c.fpl] = _b[mhs_adhd:c.fpl#c.fpl]
local sign_wgt = sign(_b[mhs_adhd:c.fpl#c.fpl]-_b[mss_asd:c.fpl#c.fpl])
display "Ho: mod3sqr <= mod4sqr  p-value = " 1-ttail(r(df_r),`sign_wgt'*sqrt(r(F)))
display "Ho: mod3sqr >= mod4sqr  p-value = " ttail(r(df_r),`sign_wgt'*sqrt(r(F)))



/* HISTOGRAM GRAPH 1 */

gen fpl_seg=autocode(fpl,15,0,450)
collapse (mean) meanadhd=adhd (mean) meanasd=asd [fweight=int_weight_op],  by(fpl_seg female)

twoway line meanadhd meanasd fpl if female==1, sort lp(solid solid) lc(blue red) name(HistFigure1) ytitle("Prevalence of diagnosis (%)") xtitle("Percentage of federal poverty level (%)") legend(rows(1) pos(6) label(1 "ADHD in girls") label(2 "ASD in girls") label(3 "ADHD in boys") label(4 "ASD in boys")) || line meanadhd meanasd fpl if female==0, lp(dash dash) lc(blue red)
/*
graph export HistFigure1.jpg, name(HistFigure1) replace
*/


