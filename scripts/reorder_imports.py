#!/usr/bin/env python3
import os

def reorder_imports(content):
    lines = content.splitlines()
    
    module_decl = None
    import_std = False
    lpc_imports = []
    other_imports = []
    
    new_lines = []
    body_start_idx = len(lines)
    
    i = 0
    while i < len(lines):
        line = lines[i].strip()
        if not line:
            i += 1
            continue
        
        if line.startswith("module ") or line.startswith("export module "):
            module_decl = line
            i += 1
            continue
        
        if line.startswith("import "):
            if line == "import std;":
                import_std = True
            elif line.startswith("import lpc."):
                lpc_imports.append(line)
            else:
                other_imports.append(line)
            i += 1
            continue
        
        body_start_idx = i
        break
    
    if module_decl:
        new_lines.append(module_decl)
        new_lines.append("")
    
    if import_std:
        new_lines.append("import std;")
        if lpc_imports or other_imports:
            new_lines.append("")
            
    if other_imports:
        other_imports.sort()
        new_lines.extend(other_imports)
        if lpc_imports:
            new_lines.append("")
            
    if lpc_imports:
        lpc_imports.sort()
        new_lines.extend(lpc_imports)
    
    if new_lines:
        new_lines.append("")
    
    body = lines[body_start_idx:]
    while body and not body[0].strip():
        body.pop(0)
    
    return "
".join(new_lines + body) + "
"

def process_file(filepath):
    with open(filepath, 'r') as f:
        content = f.read()
    
    new_content = reorder_imports(content)
    
    if content != new_content:
        with open(filepath, 'w') as f:
            f.write(new_content)
        print(f"Updated {filepath}")

def main():
    script_dir = os.path.dirname(os.path.realpath(__file__))
    project_root = os.path.dirname(script_dir)
    src_dir = os.path.join(project_root, "src")

    for root, dirs, files in os.walk(src_dir):
        for file in files:
            if file.endswith((".cpp", ".cppm")):
                process_file(os.path.join(root, file))

if __name__ == "__main__":
    main()
