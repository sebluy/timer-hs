A simple command line time tracker written in Haskell.

## Build and Install

```
cabal install exe:timer-hs --installdir=./bin --overwrite-policy=always
```

## Operations

```
timer-hs start <task>
timer-hs list <n>
timer-hs summary
timer-hs summary daily
timer-hs summary total
timer-hs summary weekly
timer-hs update <id> <task> <start> <stop>
timer-hs delete <id>
```

### TODO
- Fix bug during setup when time.log doesn't exist.
- More error checking.
- Allow update to be given any set of fields & support more datetime formats.

### FUTURE
```
timer-hs csv
```

## File Format

A log of all modifications to entries as follows:
<op> <id> <task> <start> <end>

### Example

```
Create 1 "haskell" 12345 56789
Update 1 "haskell" 90432 98798
Delete 1
```



