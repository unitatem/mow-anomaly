source("src/data_extraction.R")

resources_path = "resources/"

credit_card_file = "creditcard_final.csv"
frogs_file = "Frogs_MFCCs.csv"

credit_card_data = paste(resources_path, credit_card_file, sep = "")
frog_data = paste(resources_path, frogs_file, sep = "")

extract_normal_anomaly = function() {
  return(extract_normal_anomaly_card(credit_card_data))
}

clusters_count = 2
