import os
import pandas as pd

# Set the path to the folder containing the files
folder_path = r'C:\Users\prsyu\OneDrive\Bidlung\University\M.S. Leiden University\M.S. Neuroscience (Research)\Internship Groningen\Meta analysis Literature final\(3) Cavanna (2022)\(3) Cavanna (2022)_Attentional_Blink\(3)AB_RawData\Psilo'

# Set the path to the folder where the combined_datafile.csv will be stored
output_folder_path = r"C:\Users\prsyu\OneDrive\Bidlung\University\M.S. Leiden University\M.S. Neuroscience (Research)\Internship Groningen\Meta analysis Literature final\(3) Cavanna (2022)\(3) Cavanna (2022)_Attentional_Blink"

# Create a new file to store the combined content
output_file_path = os.path.join(output_folder_path, 'combined_datafile_Psilo.csv')

# Initialize an empty DataFrame to store the combined data
combined_data = pd.DataFrame()

# Loop through all the files in the folder
for filename in os.listdir(folder_path):
    # Check if the filename starts with 'S'
    if filename.startswith('S'):
        # Create the full path to the file
        file_path = os.path.join(folder_path, filename)

        # Read the CSV file
        data = pd.read_csv(file_path)

        # Append the data to the combined_data DataFrame
        combined_data = combined_data.append(data, ignore_index=True)

# Write the combined data to a new CSV file
combined_data.to_csv(output_file_path, index=False)

print("All 'S' files combined into 'combined_datafile.csv'")

print(data)