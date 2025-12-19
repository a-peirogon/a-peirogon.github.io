#!/usr/bin/env python3
import re

with open('templates/default.html', 'r') as f:
    content = f.read()

# Patrón para encontrar selectores CSS con comillas simples
pattern = r'(\[[a-zA-Z\*\$\^]+)=\'([^\']+)\'([ a-z]*)\]'

def replace_quotes(match):
    attr = match.group(1)
    value = match.group(2)
    flags = match.group(3)
    return f'[{attr}="{value}"{flags}]'

# Reemplazar
fixed_content = re.sub(pattern, replace_quotes, content)

with open('templates/default.html', 'w') as f:
    f.write(fixed_content)

print("Fixed CSS quotes in templates/default.html")
