IDENTIFICATION DIVISION.
PROGRAM-ID. XRP-PROCESSOR.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT BALANCE-FILE ASSIGN TO "balance.dat"
        ORGANIZATION IS LINE SEQUENTIAL
        ACCESS MODE IS SEQUENTIAL
        FILE STATUS IS FILE-STATUS.

DATA DIVISION.
FILE SECTION.
FD BALANCE-FILE.
01 BALANCE-RECORD.
    05 BALANCE-AMOUNT   PIC 9(5)V99.

WORKING-STORAGE SECTION.
01 FILE-STATUS          PIC X(2).
01 AMOUNT               PIC 9(5)V99.
01 SENDER               PIC X(20).
01 RECEIVER             PIC X(20).
01 WS-BALANCE           PIC 9(5)V99.
01 WS-OUTPUT-RECORD     PIC Z(5)9.99.  *> Formatted for display with decimal

PROCEDURE DIVISION.
MAIN-PROCEDURE SECTION.
    PERFORM INITIALIZE-BALANCE
    PERFORM GET-INPUT
    PERFORM PROCESS-TRANSACTION
    STOP RUN.

INITIALIZE-BALANCE SECTION.
    OPEN OUTPUT BALANCE-FILE
    IF FILE-STATUS = "00" OR FILE-STATUS = "35"  *> 35 = file not found
        MOVE 10000.00 TO BALANCE-AMOUNT
        MOVE BALANCE-AMOUNT TO WS-OUTPUT-RECORD
        WRITE BALANCE-RECORD FROM WS-OUTPUT-RECORD
        DISPLAY "Init File status: " FILE-STATUS
        CLOSE BALANCE-FILE
        OPEN INPUT BALANCE-FILE
        IF FILE-STATUS = "00"
            READ BALANCE-FILE
                AT END
                    DISPLAY "Init read failed, forcing default."
                    MOVE 10000.00 TO BALANCE-AMOUNT
                    MOVE BALANCE-AMOUNT TO WS-OUTPUT-RECORD
                    CLOSE BALANCE-FILE
                    OPEN OUTPUT BALANCE-FILE
                    WRITE BALANCE-RECORD FROM WS-OUTPUT-RECORD
                    CLOSE BALANCE-FILE
                NOT AT END
                    MOVE BALANCE-AMOUNT TO WS-BALANCE
            END-READ
            CLOSE BALANCE-FILE
        END-IF
    ELSE
        DISPLAY "Failed to initialize balance.dat. Status: " FILE-STATUS
    END-IF.

GET-INPUT SECTION.
    OPEN INPUT BALANCE-FILE
    IF FILE-STATUS = "00"
        READ BALANCE-FILE
            AT END
                DISPLAY "File empty, using default balance."
                MOVE 10000.00 TO WS-BALANCE
                MOVE WS-BALANCE TO BALANCE-AMOUNT
                MOVE BALANCE-AMOUNT TO WS-OUTPUT-RECORD
                CLOSE BALANCE-FILE
                OPEN OUTPUT BALANCE-FILE
                WRITE BALANCE-RECORD FROM WS-OUTPUT-RECORD
                CLOSE BALANCE-FILE
            NOT AT END
                MOVE BALANCE-AMOUNT TO WS-BALANCE
                DISPLAY "Read balance: $" WS-BALANCE
        END-READ
        CLOSE BALANCE-FILE
    ELSE
        DISPLAY "Failed to open balance.dat. Status: " FILE-STATUS
        MOVE 10000.00 TO WS-BALANCE
    END-IF
    DISPLAY "Current Balance: $" WS-BALANCE
    DISPLAY "Enter sender name: "
    ACCEPT SENDER
    DISPLAY "Enter receiver name: "
    ACCEPT RECEIVER
    DISPLAY "Enter amount to send: "
    ACCEPT AMOUNT.

PROCESS-TRANSACTION SECTION.
    IF AMOUNT IS NUMERIC
        IF AMOUNT > WS-BALANCE
            DISPLAY "Insufficient funds. Transaction cancelled."
        ELSE
            SUBTRACT AMOUNT FROM WS-BALANCE
            DISPLAY "Processing transaction..."
            DISPLAY "From: " SENDER
            DISPLAY "To: " RECEIVER
            DISPLAY "Amount: $" AMOUNT
            DISPLAY "New Balance: $" WS-BALANCE
            MOVE WS-BALANCE TO BALANCE-AMOUNT
            MOVE BALANCE-AMOUNT TO WS-OUTPUT-RECORD
            OPEN OUTPUT BALANCE-FILE
            IF FILE-STATUS = "00"
                WRITE BALANCE-RECORD FROM WS-OUTPUT-RECORD
                DISPLAY "Write File status: " FILE-STATUS
                CLOSE BALANCE-FILE
            ELSE
                DISPLAY "Failed to write balance.dat. Status: " FILE-STATUS
            END-IF
            DISPLAY "Transaction sent successfully!"
        END-IF
    ELSE
        DISPLAY "Invalid amount entered. Must be a number."
    END-IF.

END PROGRAM XRP-PROCESSOR.
