use failure::{format_err, Error};

type ChildCount = usize;

type MetaCount = usize;

#[derive(Debug)]
pub struct Header {
    child_count: ChildCount,
    meta_count: MetaCount,
}

#[derive(Debug)]
pub struct Node {
    children: Vec<Node>,
    metadata: Vec<usize>,
    header: Header,
}

#[derive(Debug)]
enum ParseHeaderState {
    ChildCount,
    MetaCount,
}

#[derive(Debug)]
enum ParseState {
    Header(ParseHeaderState),
    Metadata(usize),
}

impl Node {
    pub fn new(c_count: ChildCount, m_count: MetaCount) -> Node {
        Node {
            children: Vec::new(),
            metadata: Vec::new(),
            header: Header {
                child_count: c_count,
                meta_count: m_count,
            },
        }
    }

    pub fn needs_children(&self) -> bool {
        self.children.len() < self.header.child_count
    }

    pub fn needs_metadata(&self) -> bool {
        self.metadata.len() < self.header.meta_count
    }

    pub fn iter(&self) -> NodeIterator {
        NodeIterator { queue: vec![self] }
    }

    pub fn value(&self) -> usize {
        if self.children.is_empty() {
            self.metadata.iter().sum()
        } else {
            self.metadata
                .iter()
                .map(|&idx| {
                    if let Some(c) = self.children.get(idx - 1) {
                        c.value()
                    } else {
                        0
                    }
                })
                .sum::<usize>()
        }
    }
}

#[derive(Debug)]
pub struct NodeIterator<'a> {
    queue: Vec<&'a Node>,
}

impl<'a> Iterator for NodeIterator<'a> {
    type Item = &'a Node;

    fn next(&mut self) -> Option<&'a Node> {
        if let Some(cur) = self.queue.pop() {
            if !cur.children.is_empty() {
                self.queue.append(&mut cur.children.iter().collect());
            }

            Some(cur)
        } else {
            None
        }
    }
}

pub fn make_tree(d: &[usize]) -> Result<Node, Error> {
    let mut stack: Vec<Node> = Vec::new();
    let mut parse_state = ParseState::Header(ParseHeaderState::ChildCount);

    let mut child_count = 0;

    // 2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2
    // A----------------------------------
    //     B----------- C-----------
    //                      D-----
    for v in d {
        match parse_state {
            ParseState::Header(ParseHeaderState::ChildCount) => {
                child_count = *v;
                parse_state = ParseState::Header(ParseHeaderState::MetaCount);
            }
            ParseState::Header(ParseHeaderState::MetaCount) => {
                let meta_count = v;
                let n = Node::new(child_count, *meta_count);

                stack.push(n);

                parse_state = if *meta_count > 0 && child_count == 0 {
                    ParseState::Metadata(*meta_count)
                } else {
                    ParseState::Header(ParseHeaderState::ChildCount)
                }
            }
            ParseState::Metadata(num_entries_left) => {
                if num_entries_left > 1 {
                    if let Some(node) = stack.last_mut() {
                        node.metadata.push(*v);
                        parse_state = ParseState::Metadata(num_entries_left - 1);
                    } else {
                        return Err(format_err!("No node on stack to push meta data entry to"));
                    }
                } else {
                    // Node is done, get it from the stack
                    if let Some(mut cur_node) = stack.pop() {
                        cur_node.metadata.push(*v);

                        if let Some(parent) = stack.last_mut() {
                            parent.children.push(cur_node);

                            if parent.needs_metadata() && !parent.needs_children() {
                                // Parent has enough children, next segment has
                                // to be meta data for parent
                                parse_state = ParseState::Metadata(parent.header.meta_count);
                            } else {
                                // We pushed a child to the parent but the
                                // parent still needs more children. But we're
                                // done with the current meta data segment, so
                                // what comes next has to be another child.
                                parse_state = ParseState::Header(ParseHeaderState::ChildCount);
                            }
                        } else {
                            // Stack is empty, return current node which is root. Done.
                            return Ok(cur_node);
                        }
                    } else {
                        return Err(format_err!(
                            "Current node should have been on stack but stack is empty"
                        ));
                    }
                };
            }
        };
    }

    Err(format_err!(
        "We should have returned root from stack. Hmmm.. here's the stack"
    ))
}

pub fn run(data: &str) -> Result<String, Error> {
    let parsed: Vec<usize> = data
        .trim_end()
        .split(' ')
        .map(|v| v.parse::<usize>().unwrap())
        .collect();

    let tree = make_tree(&parsed).unwrap();

    let part1: usize = tree.iter().map(|n| n.metadata.iter().sum::<usize>()).sum();
    let part2: usize = tree.value();

    Ok(format!("{} {}", part1, part2))
}
