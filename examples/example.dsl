{
    name: "lchynn",
    // Range
    age: >= -22,
    // Exact match
    com: "infini",
    // Regular expression
    team: /dev.*/ or /doc.*/,
    // Nested object
    todo: {
        pizza: "inprogress",
    },
    // Flatten object
    todo.dsl: "inprogress",
    // Nested array
    likes: ["anime", "sports"],
}
