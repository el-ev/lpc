#!/usr/bin/env python3

import subprocess
import sys
import os
from pathlib import Path
from dataclasses import dataclass
from typing import Tuple, List, Optional


@dataclass
class TestResult:
    stdout_match: bool
    stderr_match: bool
    error_message: Optional[str] = None
    
    @property
    def success(self) -> bool:
        return self.stdout_match and self.stderr_match and not self.error_message


class CommandExecutor:
    @staticmethod
    def execute(command: str, timeout: int = 30) -> Tuple[str, str]:
        try:
            result = subprocess.run(
                command,
                shell=True,
                capture_output=True,
                text=True,
                timeout=timeout
            )
            return result.stdout.strip(), result.stderr.strip()
        except subprocess.TimeoutExpired:
            return "", "Error: Command timed out after 30 seconds"
        except Exception as e:
            return "", f"Error executing command: {str(e)}"


class FileReader:
    @staticmethod
    def read_file(filename: str) -> str:
        try:
            return Path(filename).read_text().strip()
        except FileNotFoundError:
            return ""
        except Exception:
            return ""


class CommandParser:
    @staticmethod
    def parse_command_from_file(filename: str) -> Optional[str]:
        try:
            first_line = Path(filename).read_text().splitlines()[0].strip()
        except (FileNotFoundError, IndexError):
            return None
        
        if not first_line.startswith(';'):
            return None
        return first_line[1:].strip().replace('%lpc', 'build/release/lpc').replace('%s', filename)
    


class OutputComparator:
    @staticmethod
    def compare_stdout(expected: str, actual: str) -> bool:
        return expected == actual
    
    @staticmethod
    def compare_stderr(expected: str, actual: str, scm_file: str) -> bool:
        actual_lines = [
            line.split('] ')[1].strip() 
            for line in actual.splitlines() 
            if line.startswith('[ERROR]')
        ]
        expected_lines = [line.replace('%s', scm_file) for line in expected.splitlines()]
        return actual_lines == expected_lines

class TestChecker:
    def __init__(self, verbose: bool = False):
        self.verbose = verbose
        self.executor = CommandExecutor()
        self.file_reader = FileReader()
        self.command_parser = CommandParser()
        self.comparator = OutputComparator()
    
    def check_file(self, scm_file: str) -> TestResult: 
        command = self.command_parser.parse_command_from_file(scm_file)
        if not command:
            error_msg = f"Error: Cannot parse command from {scm_file}"
            if self.verbose:
                print(error_msg)
            return TestResult(False, False, error_msg)
        
        actual_stdout, actual_stderr = self.executor.execute(command)
        actual_stdout = '\n'.join([l for l in actual_stdout.splitlines() if not l.startswith('[')])
        expected_stdout = self.file_reader.read_file(f"{scm_file}.stdout")
        expected_stderr = self.file_reader.read_file(f"{scm_file}.stderr")
        
        stdout_match = self.comparator.compare_stdout(expected_stdout, actual_stdout)
        stderr_match = self.comparator.compare_stderr(expected_stderr, actual_stderr, scm_file)
        
        if self.verbose:
            self._print_comparison_results(stdout_match, stderr_match, expected_stderr, actual_stderr, expected_stdout, actual_stdout)
        
        return TestResult(stdout_match, stderr_match)
            
    @staticmethod
    def _extract_error_lines(stderr_output: str) -> list:
        return [
            line.split('] ')[1].strip()
            for line in stderr_output.splitlines()
            if line.startswith('[ERROR]')
        ]

    def _print_comparison_results(self, stdout_match: bool, stderr_match: bool, 
                                expected_stderr: str, actual_stderr: str, expected_stdout: str, actual_stdout: str):
        if not stderr_match:
            expected_lines = expected_stderr.splitlines()
            actual_lines = self._extract_error_lines(actual_stderr)
            
            if len(actual_lines) != len(expected_lines):
                print("✗ stderr differs in number of lines")
                print(f"Expected {len(expected_lines)} lines, got {len(actual_lines)} lines")
                print(f"Expected: {expected_lines}")
                print(f"Actual: {actual_lines}")
            else:
                for i, (actual_line, expected_line) in enumerate(zip(actual_lines, expected_lines)):
                    if actual_line != expected_line:
                        print(f"✗ stderr line {i+1} differs")
                        print(f"Expected: '{expected_line}'")
                        print(f"Actual: '{actual_line}'")
        if not stdout_match:
            expected_lines = expected_stdout.splitlines()
            actual_lines = actual_stdout.splitlines()

            if len(actual_lines) != len(expected_lines):
                print("✗ stdout differs in number of lines")
                print(f"Expected {len(expected_lines)} lines, got {len(actual_lines)} lines")
            else:
                for i, (actual_line, expected_line) in enumerate(zip(actual_lines, expected_lines)):
                    if actual_line != expected_line:
                        print(f"✗ stdout line {i+1} differs")
                        print(f"Expected: '{expected_line}'")
                        print(f"Actual: '{actual_line}'")


def parse_arguments() -> Tuple[bool, str]:
    if len(sys.argv) < 2 or len(sys.argv) > 3:
        print(f"Usage: {sys.argv[0]} [--verbose] <file.scm>")
        sys.exit(1)
    
    verbose = '--verbose' in sys.argv
    filename = sys.argv[1] if sys.argv[1] != '--verbose' else sys.argv[2]
    
    if not Path(filename).exists():
        print(f"Error: File {filename} does not exist")
        sys.exit(1)
    
    return verbose, filename


def main():
    verbose, scm_file = parse_arguments()
    checker = TestChecker(verbose=verbose)
    
    result = checker.check_file(scm_file)
    
    if result.success:
        print(f"PASS {scm_file}")
    else:
        print(f"FAIL {scm_file}")
        sys.exit(1)


if __name__ == "__main__":
    main()
