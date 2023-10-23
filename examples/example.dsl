{
    name: "lchynn",
    // Range
    age: >= -22,
    // Exact match
    com: "infini",
    // Regular expression
    team: /dev.*/ or /doc.*/,
    // Nested object
    todo.1: {
        pizza: "inprogress",
    },
    // Flatten object
    todo.2.dsl: "inprogress",
    // Nested array
    likes: ["anime", "sports"],
}
