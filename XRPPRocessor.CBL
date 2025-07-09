IDENTIFICATION DIVISION.
PROGRAM-ID. XRP-PROCESSOR.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT BALANCE-FILE ASSIGN TO "balance.dat"
        ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.

FILE SECTION.
FD BALANCE-FILE.
01 BALANCE-RECORD       PIC 9(5)V99.

WORKING-STORAGE SECTION.
01 AMOUNT               PIC 9(5)V99 VALUE 0.
01 SENDER               PIC X(20).
01 RECEIVER             PIC X(20).
01 FILE-STATUS-CODE     PIC XX.
    88 FILE-OK           VALUE "00".
    88 FILE-NOT-FOUND    VALUE "35".

PROCEDURE DIVISION.

MAIN-PROCESS.
    MOVE 10000.00 TO BALANCE-RECORD.

    OPEN INPUT BALANCE-FILE.
    IF FILE-STATUS-CODE = FILE-OK
        READ BALANCE-FILE
            AT END
                CONTINUE
        END-READ
        CLOSE BALANCE-FILE
    END-IF.

    DISPLAY "Current Balance: $" BALANCE-RECORD.
    DISPLAY "Enter sender name: ".
    ACCEPT SENDER.
    DISPLAY "Enter receiver name: ".
    ACCEPT RECEIVER.
    DISPLAY "Enter amount to send: ".
    ACCEPT AMOUNT.

    IF AMOUNT IS NUMERIC
        EVALUATE TRUE
            WHEN AMOUNT > BALANCE-RECORD
                DISPLAY "Insufficient funds. Transaction cancelled."
            WHEN OTHER
                SUBTRACT AMOUNT FROM BALANCE-RECORD
                DISPLAY "Processing transaction..."
                DISPLAY "From: " SENDER
                DISPLAY "To: " RECEIVER
                DISPLAY "Amount: $" AMOUNT
                DISPLAY "New Balance: $" BALANCE-RECORD

                OPEN OUTPUT BALANCE-FILE
                WRITE BALANCE-RECORD
                CLOSE BALANCE-FILE

                DISPLAY "Transaction sent successfully!"
        END-EVALUATE
    ELSE
        DISPLAY "Invalid amount entered. Must be a number."
    END-IF.

    STOP RUN.
