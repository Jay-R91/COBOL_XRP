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
01 BALANCE-RECORD PIC X(7).  *> 7-character string for balance (e.g., "0085480")

WORKING-STORAGE SECTION.
01 FILE-STATUS          PIC X(2).
01 WS-BALANCE-CENTS     PIC 9(7) VALUE 0.  *> Default to 0 cents if not read
01 WS-BALANCE-DOLLARS   PIC 9(5)V99 VALUE 0.  *> Default to 0.00 if not read
01 WS-DISPLAY-BALANCE   PIC X(10).   *> Temporary field for formatted output
01 WS-FORMAT-BALANCE    PIC $$$,$$9.99.  *> Internal formatting without leading spaces
01 WS-RAW-BALANCE       PIC X(7).    *> Raw input from file
01 WS-BALANCE-STR       PIC X(7).    *> 7-character string
01 SENDER               PIC X(20).
01 RECEIVER             PIC X(20).
01 AMOUNT               PIC 9(5)V99. *> Transaction amount (e.g., 5.00)
01 WS-AMOUNT-CENTS      PIC 9(7).    *> Amount in cents (e.g., 500)
01 WS-AMOUNT-INPUT      PIC X(10).   *> Temporary field for raw input
01 WS-AMOUNT-VALID      PIC X(1) VALUE "N".  *> Flag for input validation
01 WS-NUM-CHECK          PIC X(10).   *> Temporary field for numeric check
01 WS-NUM-CHECK-INDEX    PIC 9(2).    *> Index for loop (0-10)
01 WS-DEBUG-MSG          PIC X(50).   *> For debugging output

PROCEDURE DIVISION.
MAIN-PROCEDURE SECTION.
    PERFORM INITIALIZE-BALANCE
    PERFORM GET-INPUT
    PERFORM PROCESS-TRANSACTION
    STOP RUN.

INITIALIZE-BALANCE SECTION.
    OPEN INPUT BALANCE-FILE
    IF FILE-STATUS = "00"
        READ BALANCE-FILE INTO WS-RAW-BALANCE
            AT END
                MOVE 0 TO WS-BALANCE-CENTS
                MOVE "No data in balance file, using 0." TO WS-DEBUG-MSG
                DISPLAY WS-DEBUG-MSG
            NOT AT END
                MOVE FUNCTION NUMVAL(WS-RAW-BALANCE) TO WS-BALANCE-CENTS
                IF WS-BALANCE-CENTS = 0 OR WS-BALANCE-CENTS > 9999999
                    MOVE "Invalid balance data, using default 8548." TO WS-DEBUG-MSG
                    DISPLAY WS-DEBUG-MSG
                    MOVE 8548 TO WS-BALANCE-CENTS
                ELSE
                    MOVE "Balance read successfully." TO WS-DEBUG-MSG
                    DISPLAY WS-DEBUG-MSG
                END-IF
            END-READ
        CLOSE BALANCE-FILE
    ELSE IF FILE-STATUS = "35"  *> File not found
        CLOSE BALANCE-FILE
        OPEN OUTPUT BALANCE-FILE
        MOVE 8548 TO WS-BALANCE-CENTS  *> Default $85.48 as 8548 cents
        MOVE WS-BALANCE-CENTS TO WS-BALANCE-STR
        MOVE WS-BALANCE-STR TO BALANCE-RECORD
        WRITE BALANCE-RECORD
        MOVE "File not found, initialized with 8548." TO WS-DEBUG-MSG
        DISPLAY WS-DEBUG-MSG
        CLOSE BALANCE-FILE
    ELSE
        MOVE "Error opening balance.dat. Status: " TO WS-DEBUG-MSG
        STRING WS-DEBUG-MSG DELIMITED BY SPACE
               FILE-STATUS DELIMITED BY SIZE
               INTO WS-DEBUG-MSG
        DISPLAY WS-DEBUG-MSG
        STOP RUN
    END-IF
    COMPUTE WS-BALANCE-DOLLARS = WS-BALANCE-CENTS / 100
    MOVE WS-BALANCE-DOLLARS TO WS-FORMAT-BALANCE
    IF WS-BALANCE-DOLLARS = 0
        MOVE "$0.00" TO WS-DISPLAY-BALANCE
    ELSE
        STRING "$" DELIMITED BY SIZE
               WS-FORMAT-BALANCE DELIMITED BY SPACE
               INTO WS-DISPLAY-BALANCE
    END-IF.

GET-INPUT SECTION.
    MOVE WS-BALANCE-DOLLARS TO WS-FORMAT-BALANCE
    IF WS-BALANCE-DOLLARS = 0
        MOVE "$0.00" TO WS-DISPLAY-BALANCE
    ELSE
        STRING "$" DELIMITED BY SIZE
               WS-FORMAT-BALANCE DELIMITED BY SPACE
               INTO WS-DISPLAY-BALANCE
    END-IF
    DISPLAY "Read balance: " WS-DISPLAY-BALANCE
    MOVE WS-BALANCE-DOLLARS TO WS-FORMAT-BALANCE
    IF WS-BALANCE-DOLLARS = 0
        MOVE "$0.00" TO WS-DISPLAY-BALANCE
    ELSE
        STRING "$" DELIMITED BY SIZE
               WS-FORMAT-BALANCE DELIMITED BY SPACE
               INTO WS-DISPLAY-BALANCE
    END-IF
    DISPLAY "Current Balance: " WS-DISPLAY-BALANCE
    DISPLAY "Enter sender name: "
    ACCEPT SENDER
    DISPLAY "Enter receiver name: "
    ACCEPT RECEIVER
    DISPLAY "Enter amount to send: "
    ACCEPT WS-AMOUNT-INPUT
    MOVE WS-AMOUNT-INPUT TO WS-NUM-CHECK
    PERFORM VARYING WS-NUM-CHECK-INDEX FROM 1 BY 1 UNTIL WS-NUM-CHECK-INDEX > 10
        IF WS-NUM-CHECK(WS-NUM-CHECK-INDEX:1) NOT = SPACE AND
           WS-NUM-CHECK(WS-NUM-CHECK-INDEX:1) NOT = "." AND
           WS-NUM-CHECK(WS-NUM-CHECK-INDEX:1) IS NOT NUMERIC
            MOVE "Y" TO WS-AMOUNT-VALID
            EXIT PERFORM
        END-IF
    END-PERFORM
    IF WS-AMOUNT-VALID = "N"
        MOVE FUNCTION NUMVAL-C(WS-AMOUNT-INPUT) TO AMOUNT
    END-IF.

PROCESS-TRANSACTION SECTION.
    IF WS-AMOUNT-VALID = "Y"
        DISPLAY "Invalid amount entered. Must be a number."
        STOP RUN
    ELSE
        COMPUTE WS-AMOUNT-CENTS = AMOUNT * 100
        DISPLAY "Debug: WS-AMOUNT-CENTS = " WS-AMOUNT-CENTS
        IF WS-AMOUNT-CENTS IS NOT NEGATIVE AND WS-AMOUNT-CENTS > WS-BALANCE-CENTS
            DISPLAY "Insufficient funds. Transaction cancelled."
        ELSE
            SUBTRACT WS-AMOUNT-CENTS FROM WS-BALANCE-CENTS
            DISPLAY "Debug: New WS-BALANCE-CENTS = " WS-BALANCE-CENTS
            IF WS-BALANCE-CENTS < 0
                ADD WS-AMOUNT-CENTS TO WS-BALANCE-CENTS
                DISPLAY "Insufficient funds. Transaction cancelled."
            ELSE
                COMPUTE WS-BALANCE-DOLLARS = WS-BALANCE-CENTS / 100
                MOVE WS-BALANCE-DOLLARS TO WS-FORMAT-BALANCE
                IF WS-BALANCE-DOLLARS = 0
                    MOVE "$0.00" TO WS-DISPLAY-BALANCE
                ELSE
                    STRING "$" DELIMITED BY SIZE
                           WS-FORMAT-BALANCE DELIMITED BY SPACE
                           INTO WS-DISPLAY-BALANCE
                END-IF
                DISPLAY "Processing transaction..."
                DISPLAY "From: " SENDER
                DISPLAY "To: " RECEIVER
                MOVE AMOUNT TO WS-FORMAT-BALANCE
                IF AMOUNT = 0
                    MOVE "$0.00" TO WS-DISPLAY-BALANCE
                ELSE
                    STRING "$" DELIMITED BY SIZE
                           WS-FORMAT-BALANCE DELIMITED BY SPACE
                           INTO WS-DISPLAY-BALANCE
                END-IF
                DISPLAY "Amount: " WS-DISPLAY-BALANCE
                MOVE WS-BALANCE-DOLLARS TO WS-FORMAT-BALANCE
                IF WS-BALANCE-DOLLARS = 0
                    MOVE "$0.00" TO WS-DISPLAY-BALANCE
                ELSE
                    STRING "$" DELIMITED BY SIZE
                           WS-FORMAT-BALANCE DELIMITED BY SPACE
                           INTO WS-DISPLAY-BALANCE
                END-IF
                DISPLAY "New Balance: " WS-DISPLAY-BALANCE
                MOVE WS-BALANCE-CENTS TO WS-BALANCE-STR
                DISPLAY "Debug: WS-BALANCE-STR = " WS-BALANCE-STR
                MOVE WS-BALANCE-STR TO BALANCE-RECORD
                CLOSE BALANCE-FILE
                OPEN OUTPUT BALANCE-FILE
                WRITE BALANCE-RECORD
                CLOSE BALANCE-FILE
                DISPLAY "Transaction sent successfully!"
            END-IF
        END-IF
    END-IF.

END PROGRAM XRP-PROCESSOR.
