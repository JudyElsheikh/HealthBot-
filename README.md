Scala Health Chatbot
====================

A conversational health assistant that supports disease diagnosis, health quizzes, and basic analytics. Built with a Scala backend and a Streamlit Python frontend, the system uses Py4J for communication between the two environments.

--------------------
INSTALLATION
--------------------

1. Requirements:
   - Java JDK 8+
   - Scala 2.13+
   - sbt (Scala Build Tool)
   - Python 3.8+
   - pip (Python package manager)

2. Setup Steps:

   a. Unzip the project file:
      - Extract the ZIP file to any directory.

   b. Install Python dependencies:
      Open a terminal and run:
      pip install streamlit
      pip install py4j

   c. Start the Scala backend:
      - Navigate to the Scala source directory.
      - Run the backend:
        sbt run

   d. Start the Streamlit frontend:
      - In another terminal, run:
        python -m streamlit run ShadiaGui.py

--------------------
USAGE
--------------------

Through the Streamlit interface, you can:

1. Symptom Checker:
   - Enter your symptoms interactively.
   - The bot asks for additional symptoms if necessary.
   - Based on symptom weights and logic, it diagnoses a disease and shows severity, urgency, and precautions.

2. Health Quizzes:
   - Answer questions about various health topics.
   - Get feedback and score instantly.

3. Analytics:
   - Placeholder for all your previous interactions,Like your scores, Login count and previous symptoms.

--------------------
ARCHITECTURE
--------------------

The system includes two parts:

1. Scala Backend:
   - Contains disease data with weights, severity, urgency.
   - Uses a Binary Search Tree (BST) to analyze symptoms.
   - Handles iterative diagnosis logic.
   - Exposes functionality via Py4J (GatewayServer).

2. Python Frontend (Streamlit):
   - User-friendly web interface built with Streamlit.
   - Communicates with Scala via Py4J.
   - Displays diagnosis results, quiz questions, and (future) analytics.

File Overview:
- ShadiaGui.py            → Main Python frontend script
- scala/                  → Scala backend source code
- data                   → Diseases, Symptoms, User info , users , Questions for quizzes

--------------------
IMPORTANT
--------------------

- Always start the Scala backend first using `sbt run` before launching the Streamlit interface.
- Make sure both `streamlit` and `py4j` are installed.


