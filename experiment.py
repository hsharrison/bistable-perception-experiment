import time
import sys
import os.path
from subprocess import check_output
from contextlib import contextmanager
from math import floor, ceil

from experimentator import Experiment
from experimentator.section import ExperimentSection
from blessed import Terminal
from pyfiglet import Figlet

FIGLET_SCALE_FACTOR = 5.9  # Figlet characters per regular character.
CONTINUOUS_JUMP_LENGTH = 0.1
X_MARGIN = 20
Y_MARGIN = 5


def run_trial(exp: Experiment, trial: ExperimentSection):
    term = exp.session_data['term']
    image = exp.session_data['stimuli'][trial.data['image']]

    show_image(term, image)
    time.sleep(exp.experiment_data['stimulus_time'])

    return {
        'response': get_response(term, trial.data['response_type'], exp.experiment_data['response_time'])
    }


def show_image(term, image):
    image_width = len(image.split('\n')[0])
    reset(term)
    multiline_print_at_location(term, image, int((term.width - image_width) // 2), 0)


def get_response(term, response_type, response_time):
    reset(term)
    start_time = time.time()
    response = 0
    first = True
    with term.cbreak():
        while time.time() - start_time < response_time:
            display_response_scale(term, response_type, response, draw_arrows=first)
            first = False
            key = term.inkey(timeout=0.1)
            if key.name == 'KEY_LEFT':
                if response_type == 'discrete':
                    response = -1
                else:
                    response -= CONTINUOUS_JUMP_LENGTH
                    if response < -1:
                        response = -1

            elif key.name == 'KEY_RIGHT':
                if response_type == 'discrete':
                    response = 1
                else:
                    response += CONTINUOUS_JUMP_LENGTH
                    if response > 1:
                        response = 1

    return response


@contextmanager
def participant_context(exp: Experiment, participant: ExperimentSection):
    term = Terminal()
    exp.session_data['term'] = term
    exp.session_data['stimuli'] = [
        jpg_to_ascii(os.path.join('stimuli', '{:02}.jpg'.format(i)), background='light', height=term.height - 2)
        for i in range(1, 16)
    ]

    with term.fullscreen():
        print(term.move(int(term.width//2), int(term.height//2)))
        response_type = participant.data['response_type']
        instructions = exp.experiment_data['instructions']
        for i, instruction in enumerate(instructions, 1):
            message = get_message(instruction, response_type)

            if message.startswith('_example'):
                show_example(term, response_type, message.split('_')[2], exp.session_data['stimuli'],
                             exp.experiment_data['example_response_message'])

            else:
                reset(term)
                wrapped_multiline_print_at_location(term, message, X_MARGIN, Y_MARGIN, int(term.width//2))
                with term.location(term.width//2, term.height - 3):
                    print('{}/{}'.format(i, len(instructions)))
                time.sleep(1)  # Ensure we don't accidentally skip a message.
                input()
        yield
        reset(term)
        print(exp.experiment_data['exit_message'])
        input()

    del exp.session_data['term']


@contextmanager
def block_context(exp: Experiment, block: ExperimentSection):
    block_no = block.data['block']
    if block_no > 1:
        term = exp.session_data['term']
        reset(term)
        print(exp.experiment_data['interblock_message'].format(block_no - 1))
        time.sleep(1)
        input()
    yield


def show_example(term: Terminal, response_type: str, example: str, stimuli, message):
    if example == 'response':
        get_response(term, response_type, 8)
        return

    show_image(term, stimuli[0 if example == 'man' else -1])
    input()

    reset(term)
    display_response_scale(term, response_type, 1 if example == 'man' else -1)
    wrapped_multiline_print_at_location(term, message.format('left' if example == 'woman' else 'right'),
                                        X_MARGIN, int(Y_MARGIN//2), int(term.width//2))
    input()
    return


def display_response_scale(term: Terminal, response_type: str, value: float, draw_arrows=True):
    figlet_width = int(term.width // FIGLET_SCALE_FACTOR)
    # We need an odd width so that the cursor can be centered.
    width = figlet_width if figlet_width % 2 else figlet_width - 1
    float_idx = 1 + (width - 2) * (value + 1) / 2
    idx = ceil(float_idx) if float_idx < 0 else floor(float_idx)

    scale = width * ['-']
    scale[idx - 1:idx + 2] = ['|', 'x', '|']
    figlet_scale = figlet(''.join(scale), width=term.width)
    figlet_scale = '\n'.join(line + (term.width - len(line)) * ' ' for line in figlet_scale.split('\n'))

    effective_width = width * FIGLET_SCALE_FACTOR
    x_margin = int((term.width - effective_width) // 2)

    scale_y = int(2 * term.height // 3) - 1
    multiline_print_at_location(term, figlet_scale, x_margin, scale_y)

    if draw_arrows:
        left_arrow = figlet('<<=')
        right_arrow = figlet('=>>')
        arrow_width = len(left_arrow.split('\n')[0])

        arrow_y = int(term.height // 3)
        left_arrow_x = x_margin + int(effective_width // 4 - arrow_width // 2)
        multiline_print_at_location(term, left_arrow, left_arrow_x, arrow_y)

        label_y = arrow_y - 1
        with term.location(left_arrow_x - 8, label_y):
            print("Looks {}like a woman's figure.".format('more ' if response_type == 'continuous' else ''))

        right_arrow_x = x_margin + int(3 * effective_width // 4 - arrow_width // 2)
        multiline_print_at_location(term, right_arrow, right_arrow_x, arrow_y)

        right_message = "Looks {}like a man's face.".format('more ' if response_type == 'continuous' else '')
        with term.location(right_arrow_x - 5, label_y):
            print(right_message)


def multiline_print_at_location(term, text, x, y):
    for i, line in enumerate(text.split('\n')):
        with term.location(x, y + i):
            print(line)


def wrapped_multiline_print_at_location(term, text, x, y, width):
    wrapped_text = term.wrap(text, width=width)
    multiline_print_at_location(term, '\n'.join(wrapped_text), x, y)


def get_message(instruction, response_type: str):
    return instruction if isinstance(instruction, str) else instruction[response_type]


def reset(term: Terminal):
    print(term.clear())
    print(term.move(0, 0))


def jpg_to_ascii(filename: str, *args, **kwargs):
    command = ['jp2a'] + list(args) + ['--{}={}'.format(*item) for item in kwargs.items()]
    return check_output(command + [filename]).decode()


def figlet(text: str, **kwargs):
    return Figlet(**kwargs).renderText(text)


def text_height(text):
    return len(text.split('\n'))


def main():
    exp = Experiment.from_yaml_file(sys.argv[1])
    exp.set_run_callback(run_trial)
    exp.set_context_manager('participant', participant_context)
    exp.set_context_manager('block', block_context)
    exp.save()
    return 0


if __name__ == '__main__':
    sys.exit(main())
