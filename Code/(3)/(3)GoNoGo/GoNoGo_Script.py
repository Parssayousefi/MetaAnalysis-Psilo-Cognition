import os

# Set the path to the folder containing the files
folder_path = 'C:/Users/prsyu/OneDrive/Bidlung/University/M.S. Leiden University/M.S. Neuroscience (Research)/Internship Groningen/Meta analysis Literature final/(3) Cavanna (2022)/(3) Cavanna (2022)_Stroop/Stroop/Stroop/Placebo'


# Set the path to the folder where the combined_datafile.txt will be stored
output_folder_path = 'C:/Users/prsyu/OneDrive/Bidlung/University/M.S. Leiden University/M.S. Neuroscience (Research)/Internship Groningen/Meta analysis Literature final/(3) Cavanna (2022)/(3) Cavanna (2022)_Stroop/Stroop/Stroop'

# Create a new file to store the combined content
output_file_path = os.path.join(output_folder_path, 'combined_datafile.txt')

with open(output_file_path, 'w') as combined_file:
    # Loop through all the files in the folder
    for filename in os.listdir(folder_path):
        # Check if the filename starts with 's'
        if filename.startswith('s'):
            # Create the full path to the file
            file_path = os.path.join(folder_path, filename)
            
            # Open the file and read its content
            with open(file_path, 'r') as source_file:
                content = source_file.read()
                
                # Append the content to the combined_datafile
                combined_file.write(content)
                combined_file.write('\n')  # Add a newline character for separation

print("All 's' files combined into 'combined_datafile.txt'")





