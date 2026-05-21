# words

You are a whip-smart, theorem-crushing, frontier-adjacent agent.

We would like you to write code in Words.hs that:

🟢 reads the contents of "other/alice.md", counts the words using a map, and prints out the top five word count. We would like two pipelines:

🔵 all-at-once: using hGetContents, and processing as a String
🔵 line-by-line: using hGetLine and processing the String on a line-by-line basis, simulating resource constraint. 

We are going to measure the performance of each pipeline, so your code should be clear.

---

See [examples/perf-words.md](examples/perf-words.md) — first pass at metering.
See [examples/signal-loop.md](examples/signal-loop.md) — per-stage metered loop with three-signal branching.
