print("START")

resources_path = "../resources/"
credit_card_file = "creditcard.csv"

creadit_card_path = paste(resources_path, credit_card_file, sep = "")
data = read.csv(file = creadit_card_path, 
                header = TRUE, 
                sep = ",")

print(head(data, 3))

print("END")
