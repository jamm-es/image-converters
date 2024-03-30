import os
import subprocess
from termcolor import colored
import shutil
from PIL import Image
from PIL import ImageChops

SAMPLE_DIR = 'bmpsuite-2.8/g'
PROGRAM_PATH = '.\\target\\debug\\image-converters.exe'
SCRATCH_DIR = 'scratch'
OUTPUT_IMAGE_NAME = 'out.bmp'
PRECONVERT_WITH_FFMPEG = True


def get_unconflicted_file_name(file_path: str) -> str:
    if not os.path.exists(file_path):
        return os.path.basename(file_path)

    folder, file_name = os.path.split(file_path)
    file_base, file_ext = os.path.splitext(file_name)
    i = 1
    while True:
        attempted_file_name = f'{file_base} ({i}){file_ext}'
        if not os.path.exists(os.path.join(folder, attempted_file_name)):
            return attempted_file_name
        i += 1


os.system('color')

try:
    shutil.rmtree(SCRATCH_DIR)
except FileNotFoundError:
    pass
os.mkdir(SCRATCH_DIR)
os.mkdir(os.path.join(SCRATCH_DIR, 'failed_conversion'))
os.mkdir(os.path.join(SCRATCH_DIR, 'failed_conversion', 'reference'))
os.mkdir(os.path.join(SCRATCH_DIR, 'failed_compare'))
os.mkdir(os.path.join(SCRATCH_DIR, 'failed_compare', 'reference'))
os.mkdir(os.path.join(SCRATCH_DIR, 'failed_compare', 'converted'))
os.mkdir(os.path.join(SCRATCH_DIR, 'failed_compare', 'diff'))
os.mkdir(os.path.join(SCRATCH_DIR, 'failed_reference'))
os.mkdir(os.path.join(SCRATCH_DIR, 'failed_reference', 'reference'))
os.mkdir(os.path.join(SCRATCH_DIR, 'failed_reference', 'converted'))

failed_conv = []
failed_comp = []
failed_ref = []

for dirpath, dirnames, filenames in os.walk(SAMPLE_DIR):
    for filename in filenames:
        reference_path = os.path.join(dirpath, filename)
        _, extension = os.path.splitext(reference_path)
        if extension != '.bmp':
            continue

        print(f'{colored("[  INIT  ]", "green", attrs=["bold"], force_color=True)} {filename} {colored(f"({os.path.join(os.getcwd(), reference_path)})", "dark_grey", force_color=True)}')
        converted_path = os.path.join(SCRATCH_DIR, OUTPUT_IMAGE_NAME)

        result = subprocess.run(f'{PROGRAM_PATH} \"{reference_path}\" {converted_path}', shell=True)
        if result.returncode != 0:
            unconflicted_filename = get_unconflicted_file_name(os.path.join(SCRATCH_DIR, 'failed_conversion', filename))
            failed_path = os.path.join(SCRATCH_DIR, 'failed_conversion', 'reference', unconflicted_filename)
            shutil.copyfile(reference_path, failed_path)
            print(f'{colored("[ FAILED ]", "red", attrs=["bold"], force_color=True)} Image failed to generate {colored(f"({os.path.join(os.getcwd(), failed_path)})", "dark_grey", force_color=True)}')
            failed_conv.append((unconflicted_filename, filename, reference_path, failed_path))
            continue

        try:
            if PRECONVERT_WITH_FFMPEG:
                subprocess.run(f'ffmpeg -hide_banner -loglevel warning -i "{converted_path}" -update true -pix_fmt rgb24 {os.path.join(SCRATCH_DIR, "conv.png")}')
                subprocess.run(f'ffmpeg -hide_banner -loglevel warning -i "{reference_path}" -update true -pix_fmt rgb24 {os.path.join(SCRATCH_DIR, "ref.png")}')
                converted_img = Image.open(os.path.join(SCRATCH_DIR, 'conv.png'))
                reference_img = Image.open(os.path.join(SCRATCH_DIR, 'ref.png'))
            else:
                converted_img = Image.open(converted_path)
                reference_img = Image.open(reference_path)

            diff_img = ImageChops.difference(converted_img.convert('RGB'), reference_img.convert('RGB'))

            if converted_img.width != reference_img.width:
                reason = f'Differing widths (reference: {converted_img.width}, converted: {reference_img.width})'
            elif converted_img.height != reference_img.height:
                reason = f'Differing heights (reference: {converted_img.height}, converted: {reference_img.height})'
            elif diff_img.getbbox():
                reason = 'Different colors'
            else:
                reason = ''

            if reason != '':
                unconflicted_filename = get_unconflicted_file_name(os.path.join(SCRATCH_DIR, 'failed_compare', 'converted', filename))
                failed_converted = os.path.join(SCRATCH_DIR, 'failed_compare', 'converted', unconflicted_filename)
                failed_reference = os.path.join(SCRATCH_DIR, 'failed_compare', 'reference', unconflicted_filename)
                failed_diff = os.path.join(SCRATCH_DIR, 'failed_compare', 'diff', unconflicted_filename)
                shutil.copyfile(converted_path, failed_converted)
                shutil.copyfile(reference_path, failed_reference)
                diff_img.save(failed_diff, format='png')
                failed_comp.append((unconflicted_filename, filename, reference_path, failed_reference, failed_converted, failed_diff, reason))
                print(colored(f'Comparison failed - reason was: {reason}', 'red', force_color=True))
                print(f'{colored("[ FAILED ]", "red", attrs=["bold"], force_color=True)} Image comparison failed {colored(f"({os.path.join(os.getcwd(), failed_converted)})", "dark_grey", force_color=True)}')

            else:
                print(f'{colored("[   OK   ]", "green", attrs=["bold"], force_color=True)} Test passed!')

            # clean up images
            if PRECONVERT_WITH_FFMPEG:
                os.remove(os.path.join(SCRATCH_DIR, 'conv.png'))
                os.remove(os.path.join(SCRATCH_DIR, 'ref.png'))

        except Exception as exception:
            print(colored(str(exception), 'red', force_color=True))
            unconflicted_filename = get_unconflicted_file_name(os.path.join(SCRATCH_DIR, 'failed_reference', 'converted', filename))
            failed_converted = os.path.join(SCRATCH_DIR, 'failed_reference', 'converted', unconflicted_filename)
            failed_reference = os.path.join(SCRATCH_DIR, 'failed_reference', 'reference', unconflicted_filename)
            shutil.copyfile(converted_path, failed_converted)
            shutil.copyfile(reference_path, failed_reference)
            failed_ref.append((unconflicted_filename, filename, reference_path, failed_reference, failed_converted))
            print(f'{colored("[ FAILED ]", "red", attrs=["bold"], force_color=True)} Reference implementation failed {colored(f"({os.path.join(os.getcwd(), failed_reference)})", "dark_grey", force_color=True)}')

print(colored('┌──────────────┐', 'cyan', attrs=['bold'], force_color=True))
print(colored('│ TEST SUMMARY │', 'cyan', attrs=['bold'], force_color=True))
print(colored('└──────────────┘', 'cyan', attrs=['bold'], force_color=True))
if len(failed_conv) + len(failed_comp) + len(failed_ref) == 0:
    print(colored('All tests passed!'))
else:
    print('Failed conversion:')
    if len(failed_conv) == 0:
        print('\tNone!')
    else:
        for unconflicted_filename, filename, reference_path, failed_path in failed_conv:
            print(f'\t{unconflicted_filename} {"" if unconflicted_filename == filename else ({colored(f"originally {filename}", "dark_grey", force_color=True)})}')
            print(f'\t\tOriginal image: {reference_path}')
            print(f'\t\tReference copy: {failed_path}')
    print('Failed comparison:')
    if len(failed_comp) == 0:
        print('\tNone!')
    else:
        for unconflicted_filename, filename, reference_path, failed_reference, failed_converted, failed_diff, reason in failed_comp:
            print(f'\t{unconflicted_filename} {"" if unconflicted_filename == filename else ({colored(f"originally {filename}", "dark_grey", force_color=True)})}')
            print(f'\t\tOriginal image: {reference_path}')
            print(f'\t\tReference copy: {failed_reference}')
            print(f'\t\tConverted image: {failed_converted}')
            if failed_diff != '':
                print(f'\t\tDifference image: {failed_diff}')
            else:
                print(f'\t\tReason: {reason}')
    print('Failed reference:')
    if len(failed_ref) == 0:
        print('\tNone!')
    else:
        for unconflicted_filename, filename, reference_path, failed_reference, failed_converted in failed_ref:
            print(f'\t{unconflicted_filename} {"" if unconflicted_filename == filename else ({colored(f"originally {filename}", "dark_grey", force_color=True)})}')
            print(f'\t\tOriginal image: {reference_path}')
            print(f'\t\tReference copy: {failed_reference}')
            print(f'\t\tConverted image: {failed_converted}')

os.remove(os.path.join(SCRATCH_DIR, OUTPUT_IMAGE_NAME))