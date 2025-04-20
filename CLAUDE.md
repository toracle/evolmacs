** 1. Project Overview

Evolmacs is a self-evolving Emacs agent that dynamically generates, tests, repairs, and integrates new Emacs Lisp functions based on user natural language descriptions.

Instead of requiring users to manually write Emacs Lisp, Evolmacs collaborates with large language models (LLMs) to automate function generation and validation inside Emacs itself.

Evolmacs grows your Emacs organically, one verified function at a time.


** 2. Core Concepts

* Self-Evolution: Evolmacs can modify and expand the user's Emacs environment without manual coding.
* Feedback Loops: Every generated function must pass its associated ERT tests. If it fails, Evolmacs will automatically retry with code fixes.
* LLM Collaboration: Evolmacs leverages external LLMs (Claude, GPT, etc.) to generate and repair code, via =llm.el=.
* Safe Execution: All Emacs Lisp evaluation is sandboxed with error handling, and persistence only occurs after successful test validation.


** 📢 Notes for Coding Agents

 - Do not giant-leap through milestones. We will use TDD for development.
 - Always check if ERT tests pass before considering a step done.
 - Carefully handle eval and test failures; capture and forward errors to the LLM.
 - Prefer structured prompts and responses (strict JSON).
 - Code must remain Emacs-native, minimal external dependency beyond llm.el.
