% database.pl
% Knowledge Base on Computer Science

% --------------------------------------------------------------------
% Section: Definitions
% --------------------------------------------------------------------
% Basic definitions of key computer science concepts

definition(algorithm, 'An algorithm is a finite sequence of instructions to solve a problem.').
definition(artificial_intelligence, 'Artificial intelligence is the field of computer science focused on creating systems capable of performing tasks that normally require human intelligence.').
definition(quantum_computing, 'Quantum computing is a branch of computing that uses principles of quantum mechanics to process information in a novel way.').
definition(data_structure, 'A data structure is a specialized format for organizing and storing data.').
definition(operating_system, 'An operating system is system software that manages computer hardware and software resources and provides common services for computer programs.').
definition(programming_language, 'A programming language is a formal language used to write instructions that can be translated into machine code and executed by a computer.').
definition(compiler, 'A compiler is a program that translates source code written in a programming language into machine code that can be executed by a computer.').
definition(interpreter, 'An interpreter is a program that directly executes instructions written in a programming language without the need for compilation.').
definition(network, 'A network is a collection of computers and other devices connected together to share resources and information.').
definition(database, 'A database is an organized collection of data, typically stored and accessed electronically from a computer system.').
definition(security, 'Security refers to the protection of computer systems and data from harm, theft, and unauthorized access.').
definition(cryptography, 'Cryptography is the practice and study of techniques for secure communication in the presence of third parties.').
definition(computer_vision, 'Computer vision is a field of artificial intelligence that enables computers to interpret and understand the visual world.').
definition(machine_learning, 'Machine learning is a subset of artificial intelligence that focuses on the development of algorithms and statistical models that enable computers to learn and make predictions based on data.').
definition(software_engineering, 'Software engineering is the application of engineering principles to the design, development, maintenance, testing, and evaluation of software systems.').
definition(web_development, 'Web development is the process of building and maintaining websites and web applications.').
definition(mobile_development, 'Mobile development is the process of creating software applications that run on mobile devices.').
definition(cloud_computing, 'Cloud computing is the delivery of computing services over the internet, allowing users to access resources and applications on-demand.').
definition(internet_of_things, 'The Internet of Things (IoT) is the network of physical devices embedded with sensors, software, and other technologies to connect and exchange data with other devices and systems over the internet.').
definition(big_data, 'Big data refers to large and complex data sets that are difficult to process using traditional data processing applications.').
definition(data_science, 'Data science is an interdisciplinary field that uses scientific methods, processes, algorithms, and systems to extract knowledge and insights from structured and unstructured data.').
definition(software_testing, 'Software testing is the process of evaluating and verifying that a software application or system meets specified requirements and quality standards.').
definition(user_experience, 'User experience (UX) design is the process of creating products that provide meaningful and relevant experiences to users.').
definition(user_interface, 'A user interface (UI) is the point of interaction between a user and a computer program or device.').
definition(algorithm_analysis, 'Algorithm analysis is the study of the performance and efficiency of algorithms, with the goal of understanding their behavior and making informed design decisions.').
definition(computer_networking, 'Computer networking is the practice of connecting and exchanging data between devices over a shared communication medium.').
definition(distributed_systems, 'Distributed systems are systems that consist of multiple computers that communicate and coordinate their actions by passing messages over a network.').
definition(networking, 'Networking is the practice of connecting computers to share resources.').

% --------------------------------------------------------------------
% Section: Hardware and Software
% --------------------------------------------------------------------
% Definitions of hardware and software components

definition(hardware, 'Hardware refers to the physical components of a computer.').
definition(software, 'Software refers to the programs and applications that run on a computer.').
definition(cpu, 'The Central Processing Unit (CPU) is the primary component of a computer that processes instructions.').
definition(memory, 'Memory, also known as RAM, is the temporary storage space used by a computer to hold data and instructions while they are being processed.').
definition(storage, 'Storage refers to the long-term storage space used by a computer to store data and programs.').
definition(ram, 'Random Access Memory (RAM) is a type of computer memory that can be accessed randomly, allowing for fast read and write operations.').
definition(hdd, 'A Hard Disk Drive (HDD) is a non-volatile storage device that stores data on magnetic disks.').
definition(ssd, 'A Solid State Drive (SSD) is a non-volatile storage device that uses flash memory to store data.').
definition(cache, 'Cache is a small, high-speed memory unit used to temporarily store frequently accessed data and instructions to improve processing speed.').
definition(gpu, 'A Graphics Processing Unit (GPU) is a specialized electronic circuit designed to accelerate the creation of images in a frame buffer intended for output to a display device.').
definition(motherboard, 'The motherboard is the main circuit board of a computer that connects and allows communication between all other components.').

% --------------------------------------------------------------------
% Section: Inventors and Historical Figures
% --------------------------------------------------------------------
% Notable figures in the history of computer science

inventor(computer, 'Charles Babbage').
inventor(internet, 'Vint Cerf').
inventor(world_wide_web, 'Tim Berners-Lee').
inventor(email, 'Ray Tomlinson').
inventor(programming_language_c, 'Dennis Ritchie').
inventor(programming_language_python, 'Guido van Rossum').
inventor(programming_language_java, 'James Gosling').
inventor(programming_language_ruby, 'Yukihiro Matsumoto').
inventor(programming_language_php, 'Rasmus Lerdorf').
inventor(programming_language_javascript, 'Brendan Eich').
inventor(operating_system_unix, 'Ken Thompson').
inventor(operating_system_linux, 'Linus Torvalds').
inventor(operating_system_windows, 'Bill Gates').
inventor(operating_system_mac_os, 'Steve Jobs').
inventor(artificial_intelligence, 'John McCarthy').

% --------------------------------------------------------------------
% Section: Programming Paradigms
% --------------------------------------------------------------------
% Listing common programming paradigms

programming_paradigms(['imperative', 'functional', 'logical', 'object-oriented']).

% --------------------------------------------------------------------
% Section: Relationships between Concepts
% --------------------------------------------------------------------
% These facts express relationships between concepts.
% For example, we can indicate that a particular algorithm may be implemented
% using one or more programming paradigms.

% Example relationships for common algorithms:
algorithm_paradigm(sorting, imperative).
algorithm_paradigm(sorting, object_oriented).
algorithm_paradigm(searching, imperative).
algorithm_paradigm(searching, functional).
algorithm_paradigm(graph_traversal, imperative).
algorithm_paradigm(graph_traversal, object_oriented).
algorithm_paradigm(dynamic_programming, imperative).
algorithm_paradigm(dynamic_programming, functional).
algorithm_paradigm(greedy_algorithm, imperative).
algorithm_paradigm(greedy_algorithm, object_oriented).

% --------------------------------------------------------------------
% Section: Composite Queries / Inference Rules
% --------------------------------------------------------------------
% A rule to combine information from multiple definitions.
% This rule retrieves the definitions of two terms and returns a comparative answer.
%
% Query Example:
% ?- compare_definitions(algorithm, data_structure, Answer).
% Expected Answer:
% "Definition of algorithm: An algorithm is a finite sequence of instructions to solve a problem.
%  Definition of data_structure: A data structure is a specialized format for organizing and storing data."

compare_definitions(Term1, Term2, Answer) :-
    definition(Term1, Def1),
    definition(Term2, Def2),
    format(string(Answer), 'Definition of ~w: ~w~nDefinition of ~w: ~w', [Term1, Def1, Term2, Def2]).
