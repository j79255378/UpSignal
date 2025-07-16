# 🚀 UpSignal - Protocol Upgrade Signaling System

## 📋 Overview

UpSignal is a decentralized governance protocol built on Stacks that enables communities to signal consensus before protocol upgrades and forks. It provides a transparent, stake-weighted voting mechanism where validators and token holders can participate in critical protocol decisions.

## ✨ Features

- 🗳️ **Stake-weighted Voting**: Vote power is determined by staked tokens
- 👥 **Validator Weighting**: Special weights for network validators
- ⏰ **Time-bound Proposals**: Configurable voting periods
- 🔒 **Consensus Threshold**: 67% approval required for proposal passage
- 📊 **Real-time Analytics**: Track voting progress and consensus
- 🛡️ **Anti-spam Protection**: Minimum stake requirements for proposals

## 🚀 Getting Started

### Prerequisites

- Clarinet CLI installed
- Stacks wallet for testing

### Installation

```bash
git clone <your-repo>
cd upsignal
clarinet check
```

### 🔧 Core Functions

#### Staking Functions
- `stake-tokens(amount)` - Stake tokens to gain voting power
- `unstake-tokens(amount)` - Withdraw staked tokens

#### Governance Functions
- `create-proposal(title, description, upgrade-hash)` - Create new upgrade proposal
- `vote-on-proposal(proposal-id, vote)` - Vote yes/no on proposals
- `finalize-proposal(proposal-id)` - Finalize voting after period ends

#### Admin Functions
- `set-validator-weight(validator, weight)` - Set validator voting weights
- `update-voting-period(blocks)` - Modify voting duration
- `update-min-stake(amount)` - Change minimum stake for proposals

### 📖 Usage Examples

#### Creating a Proposal
```clarity
(contract-call? .UpSignal create-proposal 
  "Upgrade to v2.1" 
  "Critical security update with new consensus mechanism" 
  0x1234567890abcdef...)
```

#### Voting on Proposals
```clarity
(contract-call? .UpSignal vote-on-proposal u1 true)
```

#### Checking Results
```clarity
(contract-call? .UpSignal calculate-consensus u1)
```

## 🏗️ Contract Architecture

### Data Structures
- **Proposals**: Store upgrade details, voting results, and status
- **Votes**: Track individual voting records
- **Stakes**: Manage user token stakes
- **Validator Weights**: Special voting multipliers

### Voting Mechanism
1. Users stake tokens to gain voting power
2. Proposals require minimum stake to create
3. Voting power = stake × validator weight
4. 67% threshold required for passage
5. Proposals auto-finalize after voting period

## 🔍 Read-Only Functions

- `get-proposal(id)` - Retrieve proposal details
- `get-user-stake(user)` - Check user's staked amount
- `calculate-consensus(id)` - Get voting statistics
- `get-active-proposals()` - List all active proposals

## ⚙️ Configuration

Default settings:
- Minimum proposal stake: 1,000,000 microSTX
- Voting period: 1,008 blocks (~1 week)
- Consensus threshold: 67%
- Default validator weight: 1x

## 🧪 Testing

```bash
clarinet test
```

## 🤝 Contributing

1. Fork the repository
2. Create feature branch
3. Submit pull request with tests

## 📄 License

MIT License - see LICENSE file for details

---


