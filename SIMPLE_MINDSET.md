Simple Made Easy: A Study Guide
note: I formatted the author's original gist for an easier to read version:

I. Summary and Key Concepts
Rich Hickey's "Simple Made Easy" argues for a fundamental distinction between "simple" and "easy" in software development, asserting that true simplicity (un-braided, un-interleaved) is a prerequisite for reliability and long-term maintainability, whereas "easy" (familiar, near at hand) often leads to hidden complexity.

The talk delves into the etymology of these words to highlight their precise meanings and then applies them to software constructs and design principles. Hickey advocates for a focus on objective simplicity, even if it initially feels less "easy," as it ultimately leads to more robust, understandable, and adaptable systems over time.

Key Concepts
Simple vs. Easy: The core distinction, with "simple" meaning "one fold/braid" (objective, structural) and "easy" meaning "near" (subjective, based on familiarity or proximity).

Complexity (Complecting): The act of braiding or intertwining disparate concepts or concerns, leading to systems that are difficult to understand, reason about, or change.

Composing vs. Complecting: The ideal of placing independent, simple components together (composing) versus intertwining them (complecting).

Artifact vs. Construct: Distinguishing between the inherent qualities of a programming language feature or tool (construct) and the actual running software and its long-term implications (artifact). Hickey argues that we should assess constructs based on the simplicity of the artifacts they produce.

Incidental Complexity: Complexity introduced by the choice of tools or constructs, rather than being inherent to the problem domain. Hickey equates it to "your fault."

Benefits of Simplicity: Ease of understanding, ease of change, easier debugging, increased flexibility, and independence of decisions.

Limits of Understanding: Human cognitive limitations mean we can only hold a small number of intertwined concepts in our minds at once, making complex systems inherently difficult to grasp.

Reasoning about Programs: The necessity of informal reasoning about program behavior, which is hindered by complexity, contrasting with the often-misguided reliance on "guard rail programming" (e.g., extensive testing) that doesn't fundamentally address complexity.

Policy of Abstraction (Who, What, When, Where, Why): A design approach to abstract for simplicity by disentangling concerns based on these categories.

Declarative vs. Imperative: Favoring declarative approaches (describing what rather than how) to reduce complexity.

The Choice of Simplicity: Simplicity is not an accidental outcome but a deliberate choice requiring constant vigilance and a shift in developer sensibilities away from mere "ease of use."

II. Quiz
Answer the following questions in 2-3 sentences each.

Explain the etymological difference between "simple" and "complex" as discussed in the talk.

How does Rich Hickey define "easy," and what are the three aspects of "nearness" he identifies?

Why does Hickey assert that "simple" is an objective notion while "easy" is relative?

What is "complecting" in the context of software development, and why is it considered detrimental?

Hickey argues that developers are often "infatuated" with two notions of "easy." Describe these two notions and their negative impact.

Explain the distinction between a "construct" and an "artifact" in software. Why does Hickey emphasize assessing constructs based on their artifacts?

What is "incidental complexity," and whose "fault" is it according to Hickey?

How does state in a program contribute to complexity, and what does Hickey suggest as a better alternative?

According to Hickey, what is the role of tests and refactoring in dealing with complexity, and why are they ultimately insufficient on their own?

What is "polymorphism ala carte," and why does Hickey consider it a highly desirable, yet often esoteric, tool for achieving simplicity?

III. Quiz Answer Key
"Simple" derives from "sim" and "plex," meaning "one fold" or "one braid," implying a lack of intertwining. "Complex" means "braided together" or "folded together," indicating multiple interwoven parts. This etymology highlights the structural and objective nature of simplicity.

"Easy" is defined as "to lie near" or "to be nearby." The three aspects of nearness are physical proximity (easy to obtain), familiarity (near to our understanding/skillset), and near to our capabilities (within our current abilities).

Simplicity is objective because one can visually or structurally inspect software to determine if its parts are braided together or if there are interconnections. Ease, however, is relative because what is easy for one person (due to familiarity or skill) may be hard for another.

"Complecting" means to interleave, entwine, or braid concepts or concerns within software. It is detrimental because it makes systems difficult to understand, reason about, change, and debug, leading to a combinatorial burden on human comprehension.

Developers are infatuated with the notions of easy-to-obtain (quick installation/startup) and familiar (looks like what they already know). This fixation leads to choosing tools that might be instantly usable but generate "giant hairball" artifacts, hindering learning new, potentially simpler, approaches.

A "construct" is a programming language feature or tool (e.g., a specific syntax or library). An "artifact" is the resulting running software over its lifetime, including its performance, reliability, and changeability. Hickey argues that constructs should be chosen based on the simplicity of the artifacts they produce, not merely on programmer convenience or superficial ease of use.

Incidental complexity is complexity that arises not from the problem itself, but from the tools, constructs, or design choices made during implementation. Hickey provocatively states it's "your fault" because it's a consequence of the developer's choices rather than an inherent domain challenge.

State introduces complexity by complecting value and time, making it difficult to reason about a value independently of when it was accessed or what other operations might have occurred. Hickey advocates for values (immutability) and constructs that compose values and time, like Clojure's references, to regain simplicity.

Tests and refactoring are described as "guard rails" that provide a safety net but do not fundamentally simplify the underlying system. While they help in making changes, they don't help in reasoning about the program's behavior or guiding it towards simplicity. If the system is inherently complex, these tools become less effective over the long haul.

Polymorphism ala carte (e.g., Clojure protocols, Haskell type classes) refers to the ability to independently define data structures, sets of functions (abstractions/specifications), and then connect them. This allows for genericity that is not tied to specific implementations, providing a powerful means to achieve flexibility and disentanglement, making it simpler to combine and reuse components.

IV. Essay Format Questions
Analyze Rich Hickey's central argument regarding the distinction between "simple" and "easy." Discuss how this distinction challenges conventional notions of software development and what practical implications it has for choosing tools and designing systems.

Hickey identifies several common software constructs (e.g., state, objects, inheritance, loops) as inherently "complex" due to "complecting." Choose three of these and explain in detail how they lead to complexity according to Hickey, providing examples of the "braiding" or "intertwining" they cause.

Discuss the speaker's critique of the prevailing "culture of complexity" in programming. How does he argue that this culture is self-reinforcing, and what steps does he suggest developers can take to break out of this rut?

Hickey outlines a "policy of abstraction" using "who, what, when, where, why" as a framework for designing simple systems. Explain this framework and how applying it helps to "draw away" from complexity and facilitate more modular and understandable software.

Evaluate Hickey's perspective on the role of testing and agile methodologies (like XP and sprints) in achieving software quality. To what extent does he see them as beneficial, and where does he argue their limitations lie in addressing the core problem of complexity?

V. Glossary of Key Terms
Simple: (Etymological root: sim-plex - one fold/braid) Refers to something that is un-braided, un-interleaved, or un-tangled. It is an objective quality of a system or component, characterized by having one role, one task, or one objective, and a clear focus without combining disparate things.

Complex: (Etymological root: com-plex - braided/folded together) Refers to something that is intertwined, inter-dependent, or braided together. It is the opposite of simple and creates systems that are difficult to understand, reason about, and change.

Easy: (Etymological root: Latin for "adjacent" or "to lie near") Refers to something that is near to our physical reach, near to our understanding (familiar), or near to our capabilities. It is a subjective and relative quality, often confused with simplicity.

Complect (verb): An archaic word revived by Hickey meaning "to interleave, entwine, or braid." It describes the act of introducing complexity by mixing disparate concerns.

Compose (verb): To place things together. In software, it refers to assembling independent, simple components, which is contrasted with complecting.

Construct: A specific feature, tool, or syntax provided by a programming language or library (e.g., an object, a loop, a method).

Artifact: The actual running software system, including its long-term behavior, performance, reliability, and ease of change, which is the ultimate output of using constructs.

Incidental Complexity: Complexity that arises not from the inherent nature of the problem being solved, but from the chosen tools, constructs, or implementation decisions. Hickey describes it as "your fault."

Environmental Complexity: Complexity that is inherent to the execution environment (e.g., contention for memory or CPU cycles in a shared system) and is not directly within the control of the software designer, but still impacts the artifact.

Values: Immutable data; data that does not change over time. Hickey argues that using values helps prevent complecting time and value, leading to simpler systems.

State: Mutable data; data whose value can change over time. Hickey views state as inherently complex because it "complects value in time," making reasoning difficult.

Polymorphism Ala Carte: A design approach (exemplified by Clojure protocols or Haskell type classes) where data structures, function definitions, and their connections are managed independently, allowing for flexible and un-tangled genericity.

Abstraction (Hickey's definition): To draw something away, particularly from its physical nature. It involves forming sets of functions or specifications that are small and focused, without dictating how they are implemented.

Declarative Programming: A programming paradigm that focuses on describing what a program should accomplish rather than how it should accomplish it. Hickey favors declarative approaches (e.g., SQL, Datalog, rule systems) for their simplicity compared to imperative methods.

Guard Rail Programming: A metaphor used to describe an over-reliance on safety nets like extensive testing, type checkers, and refactoring tools without fundamentally addressing the underlying complexity of the software. It helps prevent crashes but doesn't guide towards simplicity.

Sensibilities: The intuitive understanding and awareness that developers should cultivate to recognize "complecting" and interconnections in software that could be independent.