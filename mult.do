tab happy // Note that stata takes the base category as the highest frequent cat

*Building a multinominal Regression Model 

* Assuming 'sex' is a string variable, convert it to a numeric variable
encode sex, gen(sex_numeric)
encode happy, gen(happy_numeric)
encode edu, gen(edu_numeric)
encode employ, gen(employ_numeric)
encode rel_per, gen(rel_per_numeric)
encode rel_den, gen(rel_den_numeric)
encode social_class, gen(social_class_numeric)
encode people_trust, gen(people_trust_numeric)


mlogit happy_numeric i.comp i.sex_numeric age i.edu_numeric i.employ_numeric i.rel_per_numeric i.rel_den_numeric i.social_class_numeric i.people_trust_numeric

ologit happy_numeric i.comp i.sex_numeric age i.edu_numeric i.employ_numeric i.rel_per_numeric i.rel_den_numeric i.social_class_numeric i.people_trust_numeric

omodel logit happy_numeric comp sex_numeric age edu_numeric employ_numeric rel_per_numeric rel_den_numeric social_class_numeric people_trust_numeric

mlogit, rrr
mlogitgof

