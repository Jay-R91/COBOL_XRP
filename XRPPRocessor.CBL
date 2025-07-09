IDENTIFICATION DIVISION.
PROGRAM-ID. XRP-PROCESSOR.

ENVIRONMENT DIVISION.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 SENDER         PIC X(20).
01 RECEIVER       PIC X(20).
01 AMOUNT         PIC 9(5)V99.
01 BALANCE        PIC 9(5)V99 VALUE 10000.00.
01 AMOUNT-DISPLAY PIC X(30).
01 PTR            PIC 9(2) VALUE 9.

PROCEDURE DIVISION.
    DISPLAY "Enter sender name: ".
    ACCEPT SENDER.
    DISPLAY "Enter receiver name: ".
    ACCEPT RECEIVER.
    DISPLAY "Enter amount to send: ".
    ACCEPT AMOUNT.

    IF AMOUNT > BALANCE
        DISPLAY "Error: Insufficient funds. Transaction cancelled."
    ELSE
        DISPLAY "Processing transaction...".
        DISPLAY "From: " SENDER.
        DISPLAY "To: " RECEIVER.
        MOVE "Amount: $" TO AMOUNT-DISPLAY.
        STRING AMOUNT DELIMITED BY SIZE INTO AMOUNT-DISPLAY WITH POINTER PTR.
        DISPLAY AMOUNT-DISPLAY.
        DISPLAY "Transaction sent successfully!".
        SUBTRACT AMOUNT FROM BALANCE GIVING BALANCE.
        DISPLAY "Remaining balance: $".
        DISPLAY BALANCE
        .

    END-IF.

    STOP RUN.
