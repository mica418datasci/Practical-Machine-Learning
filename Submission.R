#Prediction Assignment Submission
2

3
pml_write_files = function(x){
4
        n = length(x)
5
        for(i in 1:n){
6
                filename = paste0("submission/problem_id_",i,".txt")
7
                write.table(x[i], file=filename, quote=FALSE, row.names=FALSE, col.names=FALSE)
8
        }
9
}
10

11
pml_write_files(answers)
