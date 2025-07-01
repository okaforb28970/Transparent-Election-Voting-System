# 🗳️ Transparent Election Voting System

A tamper-proof blockchain voting system built on Stacks that ensures voter eligibility, transparent vote casting, and publicly auditable results while maintaining voter privacy through cryptographic hashing.

## ✨ Features

- 🔐 **Tamper-proof voting** - All votes recorded immutably on blockchain
- 👥 **Voter eligibility management** - Controlled voter registration system  
- 🕵️ **Anonymous voting** - Vote privacy through cryptographic hashing
- 📊 **Public auditability** - All election data publicly verifiable
- ⏰ **Time-bound elections** - Automatic start/end block enforcement
- 🏆 **Automated result calculation** - Winner determination and finalization

## 🚀 Quick Start

### Prerequisites
- Clarinet CLI installed
- Stacks wallet for testing

### Installation

```bash
git clone <your-repo>
cd transparent-election-voting
clarinet console
```

## 📋 Usage Guide

### 1. Create an Election

```clarity
(contract-call? .Transparent-Election create-election 
  "Presidential Election 2024" 
  "Annual presidential election for the organization" 
  u1000) ;; Duration in blocks
```

### 2. Add Candidates

```clarity
(contract-call? .Transparent-Election add-candidate 
  u1 ;; election-id
  "Alice Johnson" 
  "Experienced leader with 10 years in governance")

(contract-call? .Transparent-Election add-candidate 
  u1 
  "Bob Smith" 
  "Innovation-focused candidate with tech background")
```

### 3. Register Voters

```clarity
;; Single voter
(contract-call? .Transparent-Election register-voter 
  u1 ;; election-id
  'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)

;; Multiple voters
(contract-call? .Transparent-Election register-multiple-voters 
  u1 
  (list 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM 
        'ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5))
```

### 4. Cast Votes

```clarity
(contract-call? .Transparent-Election cast-vote 
  u1 ;; election-id
  u1) ;; candidate-id
```

### 5. Finalize Election

```clarity
(contract-call? .Transparent-Election finalize-election u1)
```

## 🔍 Query Functions

### Check Election Status
```clarity
(contract-call? .Transparent-Election get-election u1)
(contract-call? .Transparent-Election is-election-active u1)
```

### View Candidates
```clarity
(contract-call? .Transparent-Election get-candidate u1 u1)
(contract-call? .Transparent-Election get-candidate-count u1)
```

### Verify Voter Status
```clarity
(contract-call? .Transparent-Election is-voter-eligible u1 tx-sender)
(contract-call? .Transparent-Election get-voter-status u1 tx-sender)
```

### Get Results
```clarity
(contract-call? .Transparent-Election get-election-results u1)
```

## 🔒 Security Features

- **Voter Authentication**: Only registered voters can participate
- **Double
