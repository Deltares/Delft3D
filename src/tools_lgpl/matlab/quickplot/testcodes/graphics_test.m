function graphics_test
fprintf('Opening a figure with forced "painters" rendering ... ')
f = figure('renderer','painters');
fprintf('OK\n');
fprintf('The renderer is set to "%s".\n', get(f,'renderer'))
fprintf('The renderer mode is set to "%s".\n', get(f,'RendererMode'))
pause(2)

fprintf('\nSwitching to OpenGL ... ')
set(f,'renderer','opengl')
fprintf('OK\n');
fprintf('The renderer is set to "%s".\n', get(f,'renderer'))
fprintf('The renderer mode is set to "%s".\n', get(f,'RendererMode'))
pause(2)

fprintf('\nSwitching back to painters ... ')
set(f,'renderer','painters')
fprintf('OK\n');
fprintf('The renderer is set to "%s".\n', get(f,'renderer'))
fprintf('The renderer mode is set to "%s".\n', get(f,'RendererMode'))
pause(2)

fprintf('\nAdding an axes ... ')
a = axes('Parent',f);
fprintf('OK\n');
pause(2)

fprintf('\nSwitching back to OpenGL ... ')
set(f,'renderer','opengl')
fprintf('OK\n');
fprintf('The renderer is set to "%s".\n', get(f,'renderer'))
fprintf('The renderer mode is set to "%s".\n', get(f,'RendererMode'))
pause(2)

fprintf('Adding a line ... ')
l = line('Parent',a);
fprintf('OK\n');
pause(2)

fprintf('Closing figure ... ')
delete(f);
fprintf('OK\n');
pause(2)

fprintf('Requestion OpenGL information ...\n')
opengl info