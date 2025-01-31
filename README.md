# Transparent Philanthropy Platform (TPP)

A blockchain-based donation tracking system built on Stacks that ensures transparency and accountability in philanthropic giving.

## Overview

The Transparent Philanthropy Platform enables donors to make and track donations while providing complete transparency on fund usage. Built using Clarity smart contracts on the Stacks blockchain, it creates an immutable record of all donations and their statuses.

## Features

- **Secure Donations**: Make donations using STX tokens with built-in escrow functionality
- **Donation Tracking**: Each donation gets a unique ID and maintains detailed records
- **Donor Statistics**: Track donation history and counts per donor
- **Transparent Status**: Monitor donation status from pending to completed
- **On-chain Verification**: All transactions and updates are recorded on the Stacks blockchain

## Smart Contract Functions

### Public Functions

`make-donation (amount uint) (cause (string-ascii 256))`
- Makes a new donation with specified amount and cause
- Returns the unique donation ID
- Automatically tracks donor statistics

### Read-Only Functions

`get-donation (donation-id uint)`
- Retrieves full details of a specific donation
- Returns donor address, amount, cause and current status

`get-donor-donation-count (donor principal)`
- Gets the total number of donations made by a specific donor

## Development

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet)
- Node.js
- NPM/Yarn

