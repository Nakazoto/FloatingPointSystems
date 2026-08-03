/*
 * Am27S45A PROM dumper for Arduino Nano
 *
 * Press any key at 115200 baud and the whole 2K comes back as S-records:
 *
 *      A7  1 +--u--+ 24  Vcc
 *      A6  2 |     | 23  A8
 *      A5  3 |     | 22  A9
 *      A4  4 |     | 21  A10
 *      A3  5 |     | 20  /I
 *      A2  6 |     | 19  /G
 *      A1  7 |     | 18  K
 *      A0  8 |     | 17  Q7
 *      Q0  9 |     | 16  Q6
 *      Q1 10 |     | 15  Q5
 *      Q2 11 |     | 14  Q4
 *     GND 12 +-----+ 13  Q3
 *
 *      PROM        Nano            PROM        Nano
 *       1  A7      D10             13  Q3      D5
 *       2  A6      D9              14  Q4      D6
 *       3  A5      A5              15  Q5      D7
 *       4  A4      A4              16  Q6      A6   analog
 *       5  A3      A3              17  Q7      A7   analog
 *       6  A2      A2              18  K       D8
 *       7  A1      A1              19  /G      GND
 *       8  A0      A0              20  /I      +5V
 *       9  Q0      D2              21  A10     D13
 *      10  Q1      D3              22  A9      D12
 *      11  Q2      D4              23  A8      D11
 *      12  GND     GND             24  Vcc     +5V
 *
 */

static const uint8_t ADDR_PINS[11] = { A0, A1, A2, A3, A4, A5, 9, 10, 11, 12, 13 };
static const uint8_t PIN_K = 8;

static const uint8_t  DATA_FIRST = 2;     /* Q0..Q5 on D2..D7 */
static const uint8_t  DATA_LAST  = 7;
static const uint16_t PROM_SIZE  = 2048;
static const uint8_t  REC_LEN    = 16;

static const int ADC_THRESHOLD = 512;

//Settle time for data lines
static const uint16_t T_SETUP_US  = 100;
static const uint16_t T_SETTLE_US = 100;

static uint8_t readData(void)
{
    uint8_t v = (PIND >> 2) & 0x3F;

    /* Alternating the ADC mux can carry charge on the sample-and-hold, but only
     * from a high-impedance source; a TTL output is tens of ohms. */
    if (analogRead(A6) > ADC_THRESHOLD) v |= 0x40;
    if (analogRead(A7) > ADC_THRESHOLD) v |= 0x80;
    return v;
}

static void setAddress(uint16_t addr)
{
    for (uint8_t i = 0; i < 11; i++)
        digitalWrite(ADDR_PINS[i], (addr >> i) & 1);
}

static uint8_t readByte(uint16_t addr)
{
    digitalWrite(PIN_K, LOW);
    setAddress(addr);
    delayMicroseconds(T_SETUP_US);

    digitalWrite(PIN_K, HIGH);
    delayMicroseconds(T_SETTLE_US);

    return readData();
}

static void putHex(uint8_t v)
{
    static const char digits[] = "0123456789ABCDEF";
    Serial.print(digits[v >> 4]);
    Serial.print(digits[v & 0x0F]);
}

/* The count field covers address + data + checksum */
static void putRecord(char type, uint16_t addr, const uint8_t *data, uint8_t len)
{
    uint8_t count = len + 3;
    uint8_t sum   = count + (addr >> 8) + (addr & 0xFF);

    Serial.print('S');
    Serial.print(type);
    putHex(count);
    putHex(addr >> 8);
    putHex(addr & 0xFF);

    for (uint8_t i = 0; i < len; i++) {
        putHex(data[i]);
        sum += data[i];
    }
    putHex(~sum);
    Serial.println();
}

/* Streaming (RAM is too small) */
static void dump(void)
{
    static const char header[] = "AM27S291";
    uint8_t line[REC_LEN];

    for (uint8_t i = 0; i < 4; i++)
        readByte(0);

    putRecord('0', 0, (const uint8_t *)header, sizeof(header) - 1);

    for (uint16_t a = 0; a < PROM_SIZE; a += REC_LEN) {
        for (uint8_t i = 0; i < REC_LEN; i++)
            line[i] = readByte(a + i);
        putRecord('1', a, line, REC_LEN);
    }

    putRecord('9', 0, NULL, 0);
}

void setup(void)
{
    Serial.begin(115200);

    for (uint8_t p = DATA_FIRST; p <= DATA_LAST; p++)
        pinMode(p, INPUT);
    for (uint8_t i = 0; i < 11; i++)
        pinMode(ADDR_PINS[i], OUTPUT);

    pinMode(PIN_K, OUTPUT);
    digitalWrite(PIN_K, LOW);
    setAddress(0);

    /* /32 instead of the core's /128.  Full 10-bit accuracy wants <=200kHz but
     * all we need is one threshold decision. */
    ADCSRA = (ADCSRA & ~0x07) | 0x05;

    Serial.println(F("ready"));
}

void loop(void)
{
    if (!Serial.available())
        return;

    Serial.read();
    dump();

    /* A serial monitor set to "Both NL & CR" sends two characters */
    while (Serial.available())
        Serial.read();
}
