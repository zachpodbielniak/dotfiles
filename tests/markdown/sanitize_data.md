# Sanitize Data Test File

This file contains various patterns of sensitive data to test the sanitize_data script functionality.

## Personal Identifiers

- SSN: 123-45-6789
- Email: john.doe@example.com
- Phone: (555) 123-4567
- Credit Card: 4111 1111 1111 1111
- Passport: A12345678
- Driver's License: D1234567
- TIN: 12-3456789
- Military ID: 123456789
- Voter ID: AB1234567890
- Student ID: STU987654321

## Financial Information

- Account Number: 1234567890
- IBAN: DE89 3704 0044 0532 0130 00
- SWIFT/BIC: DEUTDEFFXXX
- Routing Number: 021000021
- Bitcoin Address: 1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa
- CUSIP: 037833100
- ISIN: US0378331005
- Financial PIN: 1234
- Insurance Policy: POL123456789

## Healthcare Information

- MRN: 123456789
- Health Plan ID: XYZ1234567890
- DEA Number: AB1234563
- NPI: 1234567890
- Prescription Number: RX1234567890

## Authentication Information

- Password: P@ssw0rd123!
- API Key: sk_test_4eC39HqLyjWDarjtT1zdp7dc
- JWT: eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c
- OAuth Token: ya29.a0AfH6SMBx-Vkfgx2qbQhcZvzW0aHmMZKzm
- DB Connection String: mongodb://user:password@mongodb0.example.com:27017/admin
- SSH Private Key: -----BEGIN RSA PRIVATE KEY----- MIIEpAIBAAKCAQEA1b98Ot2... -----END RSA PRIVATE KEY-----
- Password Hash: $2a$10$N9qo8uLOickgx2ZMRZoMyeIjZAgcfl7p92ldGxad68LJZdL17lhWy

## Device Identifiers

- MAC Address: 00:1A:2B:3C:4D:5E
- IMEI: 356938035643809
- MEID: A00000491F7511
- UUID: 123e4567-e89b-12d3-a456-426614174000
- IP Address: 192.168.1.254
- Serial Number: SN123456789
- License Key: XXXX-XXXX-XXXX-XXXX

## Business Identifiers

- EIN: 12-3456789
- DUNS: 123456789
- CAGE Code: AB123

## Legal Information

- Case Number: CV-2023-12345
- Patent Number: US9,123,456

## Other Technical Data

- SIM Number: 8901234567890123456
- Base64 Data: SGVsbG8gV29ybGQhIFRoaXMgaXMgc2Vuc2l0aXZlIGRhdGEu

## Mixed Content Section

Here's a section with mixed content including normal text and sensitive data:

This is a normal paragraph with some regular text that should not be redacted. However, the following information should be redacted:
Email: security@company.com
Phone: 555-987-6543
Credit Card: 5555 5555 5555 4444

## Code Block Example

```python
# This is a code block
# The following API key should be redacted
api_key = "sk_live_51LyDxCE1G3yMU4bHHCJHDPEtEjRt8RP"
# Regular code should remain intact
def process_data(input):
    return input.upper()
```

## End of Test File

This concludes the test file for sanitize_data.