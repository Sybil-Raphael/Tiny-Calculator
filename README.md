# TinyCalc

**Author:** Sybil Raphael  
**Creation Date:** November 29, 2024

## Description

This assembly language program, `TinyCalc.asm`, implements a basic calculator with the following features:

* Arithmetic Operations: The calculator supports addition, subtraction, multiplication, division, and modulo operations.
* Running Total: The program maintains a running total of all calculations.
* Session Report: At the end of a session, the program displays a report that includes:
    * The number of each type of operation performed.
    * The total of all operations.
    * The average of all operations.

## How to Run

To run the `TinyCalc.asm` program, you will need an assembler (MASM) and a way to run the resulting executable. 

1. **Link the object file:** Link the object file with the necessary libraries to create an executable. For example, using GCC:

    ```bash
    gcc TinyCalc.o -o TinyCalc
    ```
2.  **Run the executable:** Execute the resulting program:

    ```bash
    ./TinyCalc
    ```
