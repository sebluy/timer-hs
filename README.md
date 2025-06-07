# Operations

## start <task>
Begins task.
Prints time spend every 10 minutes.

## stop <task>
Ends task.
Prints time spent during session

## list <n>
Prints a list of most recent n entries.

## update <id> <task> <start> <stop>
Updates an entry by id.

## delete <id>
Deletes an entry by id.

## summary daily
Prints a summary of daily time spent on each task.

## summary weekly
Prints a summary of weekly time spent on each task.

## summary monthly
Prints a summary of monthly time spent on each task.

# File Format

A log of all modifications to entries as follows:
<op> <id> <task> <start> <end>

## Example

Create 1 "haskell" 2025-06-04T10:00:00 <null>
Update 1 "haskell" 2025-06-04T10:00:00 2025-06-04T11:00:00
Delete 1



