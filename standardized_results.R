#scripts for calculating the normalized scores in chapter 4.5
#formula:    Xscore = (Xi - Xmin) / (Xmax - Xmin)



#chapter 4.1.3
a_severity = 8
e_severity = 12
r_severity = 19
t_severity = 7
u_severity = 8

xmin = t_severity
xmax = r_severity
under = xmax - xmin 


#appium severity
(a_severity - xmin) / under * 5
#espresso_severity
(e_severity - xmin) / under * 5
#robotium severity
(r_severity - xmin) / under * 5
#tau severity
(t_severity - xmin) / under * 5
#uiautomator severity
(u_severity - xmin) / under * 5


#chapter 4.2.4, execution time
a_time = 886.79
e_time = 179.72
r_time = 488.73
t_time = 598.5
u_time = 536.04

xmin = e_time
xmax = a_time
under = xmax - xmin

#appium time
(a_time - xmin) / under * 5
#espresso time
(e_time - xmin) / under * 5
#robotium time
(r_time - xmin) / under * 5
#tau time
(t_time - xmin) / under * 5
#uiautomator time
(u_time - xmin) / under * 5


#chapter 4.3.1, lines of code
a_lines = 893
e_lines = 885
r_lines = 911
t_lines = 708
u_lines = 864
  
xmin = t_lines
xmax = r_lines
under = xmax - xmin

#appium lines
(a_lines - xmin) / under * 5
#espresso lines
(e_lines - xmin) / under * 5
#robotium lines
(r_lines - xmin) / under * 5
#tau lines
(t_lines - xmin) / under * 5
#uiautomator lines
(u_lines - xmin) / under * 5

#chapter 4.3.2, waits

a_waits = 19
e_waits = 0
r_waits = 13
t_waits = 2
u_waits = 63

xmin = e_waits
xmax = u_waits
under = xmax-xmin

#appium waits
(a_waits - xmin) / under * 5
#espresso waits
(e_waits - xmin) / under * 5
#robotium waits
(r_waits - xmin) / under * 5
#tau waits
(t_waits - xmin) / under * 5
#uiautomator waits
(u_waits - xmin) / under * 5


#chapter 4.4.4, reliability
a_fail = 0.64
e_fail = 0.08
r_fail = 0.32
t_fail = 0
u_fail = 1.74

xmin = t_fail
xmax = u_fail
under = xmax-xmin

#appium fail
(a_fail - xmin) / under * 5
#espresso fail
(e_fail - xmin) / under * 5
#robotium fail
(r_fail - xmin) / under * 5
#tau fail
(t_fail - xmin) / under * 5
#uiautomator fail
(u_fail - xmin) / under * 5


