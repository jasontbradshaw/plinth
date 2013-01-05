import os
import readline
import sys

class Interpreter:

    def __init__(self, complete_key='tab', prompt='> ', stdin=None, stdout=None):
        '''
        Instantiate a simple line-oriented interpreter framework.

        The optional argument 'complete_key' is the readline name of a
        completion key; it defaults to the Tab key ('tab'). If complete_key is
        not None, command completion is done by the 'complete()' method. The
        optional arguments stdin and stdout specify alternate input and output
        file objects; if not specified, sys.stdin and sys.stdout are used.
        '''

        # key used to trigger autocomplete
        self.complete_key = complete_key

        self.stdin = stdin or sys.stdin
        self.stdout = stdout or sys.stdout

        # the prompt issued to the user to gather input
        self.prompt = prompt

        # the intro displayed before the first prompt is issued
        self.intro = None

    def repl(self, intro=None):
        '''
        Repeatedly issue a prompt, accept input, and dispatch that input to the
        'cmd()' method.
        '''

        self.pre_loop()

        # if a completion key is defined, set up the autocompleter
        old_completer = None
        if self.complete_key:
            # store the existing completer, then revert after we're done
            old_completer = readline.get_completer()
            readline.set_completer(self.__complete)
            readline.parse_and_bind(self.complete_key + ': complete')

        try:
            # write an intro if specified
            intro = intro if intro is not None else self.intro
            if intro is not None:
                self.stdout.write(str(intro) + os.linesep)

            self.post_intro(intro)

            stop = None
            while not stop:
                self.pre_prompt()

                try:
                    line = raw_input(self.prompt)
                except EOFError, e:
                    line = e
                except KeyboardInterrupt, e:
                    line = e

                line = self.pre_cmd(line)
                stop = self.cmd(line)
                stop = self.post_cmd(stop, line)

            self.post_loop()

        # unset the autocompleter, if necessary
        finally:
            if self.complete_key:
                readline.set_completer(old_completer)

    def post_intro(self, intro):
        '''
        Method called once directly after the intro is written, but before the
        loop has started. Does nothing by default.
        '''
        pass

    def pre_prompt(self):
        '''
        Method called immediately before the prompt is issued to the user.
        '''
        pass

    def pre_cmd(self, line):
        '''
        Hook method executed just before the command is interpreted, but after
        input is received from the prompt. Receives 'line' as the unmodified
        user input, and can return a modified value if desired. Passes the value
        through unchanged by default.
        '''
        return line

    def cmd(self, line):
        '''
        Hook method for the command that interprets user input. Output can be
        written via self.stdout. Returns a single boolean indicating whether the
        repl() method should end. Does nothing by default.
        '''
        return False

    def post_cmd(self, stop, line):
        '''
        Hook method executed just after a command is finished being interpreted.
        Receives 'stop' from the command interpretation, and the 'line' that was
        interpreted. Returns a new value indicating whether the repl() method
        should end.
        '''
        return stop

    def pre_loop(self):
        '''
        Hook method executed once when the repl() method is called, before the
        intro is generated. Does nothing by default.
        '''
        pass

    def post_loop(self):
        '''
        Hook method executed once when the repl() method is about to return.
        Does nothing by default.
        '''
        pass

    def complete(self, line):
        '''
        Receives line of user input and returns a tuple of lines that the given
        line might complete to, in no particular order. Returns an empty tuple
        by default.
        '''
        return ()

    def __complete(self, line, state):
        '''
        Return the next possible completion for 'line'. This function is passed
        to readline, and uses the user-facing 'complete()' to get a list of
        completions.
        '''

        # get the completion possibilities on the first iteration
        if state == 0:
            self.__completion_matches = self.complete(line)

        try:
            return self.__completion_matches[state]
        except IndexError:
            return None
