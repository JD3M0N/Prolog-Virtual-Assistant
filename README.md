# Simple Virtual Assistant

## Project Overview

This project is a simple virtual assistant designed to answer questions about a specific topic (e.g., history, science, etc.). The assistant uses a database of facts and rules to process user queries written in natural language and provide appropriate responses.

## Implementation

The virtual assistant is implemented in **Prolog**. Prolog's strengths in representing knowledge through facts and rules, along with its powerful pattern matching and backtracking capabilities, make it an ideal choice for this project. 

Key implementation details include:

- **Knowledge Base**: A collection of facts and rules that encapsulate the information about the chosen topic.
- **Query Processing**: A simple parser that converts user input in natural language into Prolog queries.
- **Inference Engine**: Utilizes Prolog’s built-in logical inference to resolve queries based on the defined knowledge base.

This design allows for a modular and straightforward approach to expanding the assistant’s knowledge and capabilities.

## Team Members

- **Gabriel Alonso Coro** - Group C312
- **Josue Rolando Naranjo Sieiro** - Group C311

## How to Run

1. **Install SWI-Prolog** (or your preferred Prolog environment).
2. **Clone the repository**:
   ```bash
   git clone https://github.com/yourusername/virtual-assistant.git
    ```
3. **Navigate to the project directory:**
    ```bash
   cd virtual-assistant
    ```
4.  **Load the main Prolog file in your Prolog interpreter:**
    ```bash
    ?- [main].
    ```
5. **Start interacting with the virtual assistant by entering your questions.**

## Future Enhancements
- Enhance the natural language processing capabilities to handle a wider range of queries.
- Expand the knowledge base with more topics and detailed facts.
- Improve the parser to handle more complex sentence structures.

  Feel free to explore the repository and contribute to the project. For any questions or issues, please open an issue on GitHub.
