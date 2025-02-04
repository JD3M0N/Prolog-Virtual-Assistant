% database.pl
% Knowledge Base on Computer Science


:- module(database, [
    definition/2,
    inventor/2,
    programming_paradigms/1,
    algorithm_paradigm/2,
    compare_definitions/3
    % Add more predicates here as needed
]).


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
definition(variable, 'A variable is a storage location paired with an associated symbolic name, which contains some known or unknown quantity of information referred to as a value.').
definition(function, 'A function is a block of organized, reusable code that is used to perform a single, related action.').
definition(recursion, 'Recursion is a method of solving a problem where the solution depends on solutions to smaller instances of the same problem.').
definition(loop, 'A loop is a sequence of instructions that is continually repeated until a certain condition is reached.').
definition(array, 'An array is a data structure consisting of a collection of elements, each identified by at least one array index or key.').
definition(stack, 'A stack is a linear data structure which follows a particular order in which the operations are performed. The order may be LIFO (Last In First Out) or FILO (First In Last Out).').
definition(queue, 'A queue is a collection of entities that are maintained in a sequence and can be modified by the addition of entities at one end of the sequence and the removal of entities from the other end.').
definition(tree, 'A tree is a widely used abstract data type that simulates a hierarchical tree structure, with a root value and subtrees of children with a parent node, represented as a set of linked nodes.').
definition(graph, 'A graph is a collection of nodes connected by edges.').
definition(hash_table, 'A hash table is a data structure that implements an associative array abstract data type, a structure that can map keys to values.').
definition(algorithm_analysis, 'Algorithm analysis is the determination of the amount of time and space resources required to execute it.').
definition(big_o_notation, 'Big O notation is a mathematical notation that describes the limiting behavior of a function when the argument tends towards a particular value or infinity.').
definition(object_oriented_programming, 'Object-oriented programming is a programming paradigm based on the concept of objects, which can contain data and code to manipulate the data.').
definition(class, 'A class is a blueprint for creating objects, providing initial values for state and implementations of behavior.').
definition(object, 'An object is an instance of a class.').
definition(inheritance, 'Inheritance is a mechanism in which one class acquires the property of another class.').
definition(polymorphism, 'Polymorphism is the provision of a single interface to entities of different types.').
definition(encapsulation, 'Encapsulation is the bundling of data with the methods that operate on that data.').
definition(abstraction, 'Abstraction is the concept of hiding the complex reality while exposing only the necessary parts.').
definition(software_development_life_cycle, 'The software development life cycle is a process for planning, creating, testing, and deploying an information system.').
definition(agile_methodology, 'Agile methodology is a practice that promotes continuous iteration of development and testing throughout the software development life cycle of the project.').
definition(version_control, 'Version control is a system that records changes to a file or set of files over time so that you can recall specific versions later.').
definition(repository, 'A repository is a central location in which data is stored and managed.').
definition(branch, 'A branch is an independent line of development in version control.').
definition(merge, 'Merge is a version control operation that integrates changes from different branches.').
definition(commit, 'A commit is a record of changes made to a repository.').
definition(push, 'Push is a version control operation that sends changes to a remote repository.').
definition(pull, 'Pull is a version control operation that retrieves changes from a remote repository.').
definition(merge_conflict, 'A merge conflict occurs when two branches have changed the same part of a file, and the changes cannot be automatically merged.').
definition(software_architecture, 'Software architecture is the process of defining a structured solution that meets all technical and operational requirements while optimizing common quality attributes such as performance, security, and manageability.').
definition(client_server_architecture, 'Client-server architecture is a computing model in which the server hosts, delivers, and manages most of the resources and services to be consumed by the client.').
definition(peer_to_peer_architecture, 'Peer-to-peer architecture is a distributed application architecture that partitions tasks or workloads between peers.').
definition(microservices_architecture, 'Microservices architecture is an architectural style that structures an application as a collection of loosely coupled services.').
definition(monolithic_architecture, 'Monolithic architecture is a traditional unified model for designing software as a single unit with three main components: a client-side user interface, a server-side application, and a database.').
definition(restful_architecture, 'RESTful architecture is an architectural style that uses a stateless, client-server, cacheable communications protocol, such as HTTP, to create scalable and reliable web services.').
definition(mvc_architecture, 'Model-View-Controller (MVC) is a software architectural pattern that separates an application into three main logical components: the model, the view, and the controller.').
definition(design_pattern, 'A design pattern is a general repeatable solution to a commonly occurring problem in software design.').
definition(creational_pattern, 'Creational patterns provide object creation mechanisms that increase flexibility and reuse of existing code.').
definition(structural_pattern, 'Structural patterns explain how to assemble objects and classes into larger structures while keeping the structures flexible and efficient.').
definition(behavioral_pattern, 'Behavioral patterns focus on how objects distribute work, how objects communicate, and how objects encapsulate behavior to enhance program flexibility.').
definition(singleton_pattern, 'The Singleton pattern ensures a class has only one instance and provides a global point of access to it.').
definition(factory_pattern, 'The Factory pattern defines an interface for creating objects but lets subclasses alter the type of objects that will be created.').
definition(abstract_factory_pattern, 'The Abstract Factory pattern provides an interface for creating families of related or dependent objects without specifying their concrete classes.').
definition(builder_pattern, 'The Builder pattern constructs complex objects step by step, producing different types and representations of an object using the same construction code.').
definition(prototype_pattern, 'The Prototype pattern creates new objects by copying an existing object, known as the prototype.').
definition(adapter_pattern, 'The Adapter pattern allows objects with incompatible interfaces to work together by providing a wrapper with the required interface.').
definition(bridge_pattern, 'The Bridge pattern decouples an abstraction from its implementation so that the two can vary independently.').
definition(composite_pattern, 'The Composite pattern composes objects into tree structures to represent part-whole hierarchies.').

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
definition(power_supply, 'The power supply unit (PSU) is a hardware component that provides electrical power to a computer.').
definition(input_device, 'An input device is a peripheral device used to provide data and control signals to a computer.').
definition(output_device, 'An output device is a peripheral device used to receive data and control signals from a computer.').
definition(peripheral, 'A peripheral is an auxiliary device used to put information into and get information out of the computer.').
definition(network_device, 'A network device is a hardware component used to connect computers and other devices to create a network.').
definition(keyboard, 'A keyboard is an input device that uses a set of keys or buttons to send data to a computer.').
definition(mouse, 'A mouse is an input device that controls the movement of a cursor on a computer screen.').
definition(scanner, 'A scanner is an input device that captures images and text from documents and converts them into digital data.').
definition(printer, 'A printer is an output device that produces text and graphics on paper.').
definition(speaker, 'A speaker is an output device that produces sound generated by a computer.').
definition(monitor, 'A monitor is an output device that displays visual information generated by a computer.').
definition(modem, 'A modem is a network device that modulates and demodulates digital data for transmission over communication lines.').
definition(router, 'A router is a network device that forwards data packets between computer networks.').
definition(switch, 'A switch is a network device that connects devices within a local area network (LAN) and forwards data packets between them.').
definition(laptop, 'A laptop is a portable computer designed for mobile use.').
definition(desktop, 'A desktop computer is a personal computer designed for use on a desk or table.').
definition(server, 'A server is a computer or system that provides resources, data, services, or programs to other computers, known as clients, over a network.').
definition(mainframe, 'A mainframe is a large, high-performance computer used for bulk data processing and serving many users simultaneously.').
definition(supercomputer, 'A supercomputer is a computer with a high level of performance compared to a general-purpose computer.').

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
inventor(turing_machine, 'Alan Turing').
inventor(relational_database, 'Edgar F. Codd').
inventor(compiler, 'Grace Hopper').
inventor(ethernet, 'Robert Metcalfe').
inventor(public_key_cryptography, 'Whitfield Diffie and Martin Hellman').
inventor(quantum_computing, 'David Deutsch').
inventor(algorithm_analysis, 'Donald Knuth').
inventor(operating_system_multics, 'Fernando J. Corbató').
inventor(operating_system_cp_m, 'Gary Kildall').
inventor(operating_system_ms_dos, 'Tim Paterson').
inventor(programming_language_fortran, 'John Backus').
inventor(programming_language_lisp, 'John McCarthy').
inventor(programming_language_algol, 'Friedrich L. Bauer').
inventor(programming_language_pascal, 'Niklaus Wirth').
inventor(programming_language_perl, 'Larry Wall').
inventor(programming_language_swift, 'Chris Lattner').
inventor(programming_language_go, 'Robert Griesemer, Rob Pike, and Ken Thompson').
inventor(programming_language_rust, 'Graydon Hoare').
inventor(programming_language_kotlin, 'JetBrains').
inventor(programming_language_scala, 'Martin Odersky').
inventor(programming_language_haskell, 'Simon Peyton Jones and Philip Wadler').
inventor(programming_language_erlang, 'Joe Armstrong').
inventor(programming_language_elixir, 'José Valim').
inventor(programming_language_dart, 'Lars Bak and Kasper Lund').
inventor(programming_language_clojure, 'Rich Hickey').
inventor(programming_language_fsharp, 'Don Syme').
inventor(programming_language_ocaml, 'Xavier Leroy').
inventor(programming_language_scheme, 'Guy L. Steele and Gerald Jay Sussman').
inventor(programming_language_ml, 'Robin Milner').
inventor(programming_language_prolog, 'Alain Colmerauer and Robert Kowalski').

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
algorithm_paradigm(searching, object_oriented).
algorithm_paradigm(searching, declarative).
algorithm_paradigm(graph_traversal, functional).
algorithm_paradigm(graph_traversal, declarative).
algorithm_paradigm(dynamic_programming, object_oriented).
algorithm_paradigm(dynamic_programming, declarative).
algorithm_paradigm(greedy_algorithm, functional).
algorithm_paradigm(greedy_algorithm, declarative).
algorithm_paradigm(backtracking, imperative).
algorithm_paradigm(backtracking, functional).
algorithm_paradigm(backtracking, object_oriented).
algorithm_paradigm(backtracking, declarative).
algorithm_paradigm(divide_and_conquer, imperative).
algorithm_paradigm(divide_and_conquer, functional).
algorithm_paradigm(divide_and_conquer, object_oriented).
algorithm_paradigm(divide_and_conquer, declarative).
algorithm_paradigm(branch_and_bound, imperative).
algorithm_paradigm(branch_and_bound, functional).
algorithm_paradigm(branch_and_bound, object_oriented).
algorithm_paradigm(branch_and_bound, declarative).
algorithm_paradigm(heuristic, imperative).
algorithm_paradigm(heuristic, functional).
algorithm_paradigm(heuristic, object_oriented).
algorithm_paradigm(heuristic, declarative).
algorithm_paradigm(approximation, imperative).
algorithm_paradigm(approximation, functional).
algorithm_paradigm(approximation, object_oriented).
algorithm_paradigm(approximation, declarative).

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
