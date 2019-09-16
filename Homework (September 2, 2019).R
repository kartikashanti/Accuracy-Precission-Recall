# Confusion Matrix
# Kita menggunakan data titanic
library(readxl)
Data <- read_excel("Data.xlsx")
View(Data)

# Membuat confusion matrix dengan fungsi table()
table(Data$Aktual,Data$Prediksi)

# Parameter confusion matrix 
# diassign sebagai conf 
conf <- table(Data$Aktual,Data$Prediksi)
conf

# Pendefinisian baru yaitu sebagai berikut:  
AA <- conf[1, 1] 
AB <- conf[1, 2] 
AC <- conf[1, 3]
BA <- conf[2,1] 
BB <- conf[2,2] 
BC <- conf[2,3]
CA <- conf[3,1]
CB <- conf[3,2]
CC <- conf[3,3]

# Menghitung akurasi 
acc <- (AA+BB+CC)/(AA+AB+AC+BA+BB+BC+CA+CB+CC)
acc

# Menghitung presisi
# Presisi A (Dalam hal ini TP dari A adalah AA, dan FP adalah BA dan CA)
prec <- AA / (AA+BA+CA)
prec

# Presisi B (Dalam hal ini TP dari B adalah BB, dan FP adalah AB dan CB)
prec <- BB / (AB+BB+CB)
prec

# Presisi C (Dalam hal ini TP dari C adalah CC, dan FP adalah AC dan BC)
prec <- CC / (AC+BC+CC)
prec

# Menghitung recall
# Recall A (Dalam hal ini TP dari A adalah AA, dan FN adalah AB dan AC)
rec <- AA / (AA+AB+AC)
rec

# Recall B (Dalam hal ini TP dari B adalah BB, dan FN adalah BA dan BC)
rec <- BB / (BA+BB+BC)
rec

# Recall C (Dalam hal ini TP dari C adalah CC, dan FN adalah CA dan CB)
rec <- CC / (CA+CB+CC)
rec

