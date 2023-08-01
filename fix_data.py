import re
import sys

def fix_csv(file_path):
    with open(file_path, 'r') as file:
        content = file.read()

    fixed_content = ""
    inside_curly_brackets = False
    line_start = 0
    curly = 0

    for i in range(len(content)):
        if content[i] == "{":
            inside_curly_brackets = True
            curly +=1
            print("curly number " + str(curly))

        if content[i] == "}":
            inside_curly_brackets = False
            fixed_content += content[line_start:i+1]
            line_start = i+1

        if not inside_curly_brackets and content[i:i+2] == '""' and content[i+2] not in (',', ':') and content[i:i+4] != '""""':
            fixed_content += content[line_start:i] + '"\n'
            line_start = i+1


    with open(sys.argv[2], 'x') as file:
        file.write(fixed_content)

# Example usage
csv_file_path = sys.argv[1]
fix_csv(csv_file_path)
print("CSV file has been fixed!")
