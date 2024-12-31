#let colors = (
  header: rgb("#f0f0f0"),
  border: rgb("#cccccc"),
  alternate: rgb("#f9f9f9"),
)

// Import and process memory data
#let raw_data = read("memory.csv").split("\n")
#let headers = raw_data.first().split(",").map(h => h.replace("\"", "").trim())

// Filter out trial rows and format data
#let memory_scores = {
  let exclude = ("Trial 1 Correct", "Trial 2 Correct", "Trial 3 Correct", "Trial 4 Correct")

  let data = raw_data
    .slice(1)
    .filter(line => line.trim() != "")
    .map(line => {
      let values = line.split(",").map(v => v.replace("\"", "").trim())
      let row = (:)
      for (i, h) in headers.enumerate() {
        if i < values.len() {
          row.insert(h, values.at(i))
        }
      }
      row
    })

  data
    .filter(row => {
      let scale = row.at("scale", default: "")
      not exclude.any(pattern => scale.contains(pattern))
    })
    .map(row => (
      row.at("scale", default: ""),
      row.at("score", default: ""),
      row.at("percentile", default: "") + if row.at("percentile", default: "") != "" { "th" } else { "" },
      row.at("range", default: ""),
    ))
}

// Table creation function
#let create_table(scores, caption: none) = {
  figure(
    table(
      columns: (auto, auto, auto, auto),
      inset: 8pt,
      align: (left, center, center, left),
      stroke: colors.border,
      fill: (_, row) => if row == 0 { colors.header } else if calc.odd(row) { colors.alternate } else { white },
      // Header
      [*Scale*], [*Score*], [*Percentile*], [*Range*],
      // Data rows
      ..scores.flatten()
    ),
    caption: caption,
    kind: "memory-scores",
    supplement: "Table",
  )
}

= Memory
<sec-memory>

// Core Memory Profile with table
== Core Memory Profile
<core-memory-profile>

#create_table(memory_scores, caption: [Memory Test Performance])

#block(
  width: 100%,
  inset: (x: 1em, y: 0.5em),
  fill: colors.alternate,
  radius: 4pt,
)[
  - Below Average initial encoding efficiency
  - Variable retention across delay intervals
  - High Average visual information maintenance
  - Selective recognition accuracy deficits
]

// Performance Patterns section
== Performance Patterns
<performance-patterns>

=== Learning Efficiency
<learning-efficiency>
#block(
  width: 100%,
  inset: (x: 1em, y: 0.5em),
)[
  - Below Average first-trial acquisition
  - Gradual improvement to Average range with repetition
  - Low Average complex single-trial learning
]

=== Delayed Recall
<delayed-recall>
#block(
  width: 100%,
  inset: (x: 1em, y: 0.5em),
)[
  - High Average visual information maintenance
  - Average story detail preservation
  - Intact delayed recall for structured material
]

=== Recognition Memory
<recognition-memory>
#block(
  width: 100%,
  inset: (x: 1em, y: 0.5em),
)[
  - Average discriminability indices
  - Selective weakness in distractor differentiation
  - Enhanced performance with contextual cues
]

// Functional Implications section
== Functional Implications
<functional-implications>
#block(
  width: 100%,
  inset: (x: 1em, y: 0.5em),
  fill: colors.alternate,
  radius: 4pt,
)[
  - Challenges with rapid information acquisition
  - Preserved long-term storage mechanisms
  - Optimal performance with structured learning opportunities
  - Enhanced retention for visual versus verbal material
  - Overall intact consolidation mechanisms with specific vulnerabilities in initial encoding
]

// Domain visualization
#let domain(title: none, file_qtbl, file_fig) = {
  grid(
    columns: (1fr, 1fr),
    gutter: 1em,
    create_table(memory_scores, caption: [#title Scores]),
    figure(
      image(file_fig, width: 100%),
      caption: [
        _Learning efficiency and delayed recall_ refer to the rate and ease with which new information (e.g., facts, stories, lists, faces, names) can be encoded, stored, and later recalled from long-term memory.
      ],
      kind: "memory-figure",
      supplement: "Figure",
    ),
  )
}

#domain(
  title: "Memory",
  "fig_memory.svg",
  "table_memory.png",
)
