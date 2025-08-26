# PYTHONSTARTUP script to fix matplotlib backend for euporie
import os
import sys

# Only run in euporie context
if ('euporie' in sys.executable.lower() or 
    os.getenv('EUPORIE_GRAPHICS') or 
    os.getenv('TERM') == 'xterm-kitty'):
    
    print("=== EUPORIE MATPLOTLIB FIX ACTIVE ===")
    
    # Configure matplotlib to NOT use Agg backend
    import matplotlib
    
    # Get current backend
    current_backend = matplotlib.get_backend()
    print(f"Default matplotlib backend: {current_backend}")
    
    # Only change backend if it's the problematic Agg backend
    if current_backend == 'Agg':
        print("Agg backend detected - switching to inline-compatible backend")
        
        # Try backends in order of preference for euporie
        backends_to_try = [
            'module://matplotlib_inline.backend_inline',
            'nbagg', 
            'webagg'
        ]
        
        for backend in backends_to_try:
            try:
                matplotlib.use(backend)
                print(f"Successfully set backend to: {backend}")
                break
            except (ImportError, RuntimeError) as e:
                print(f"Backend {backend} not available: {e}")
                continue
    else:
        print(f"Backend {current_backend} is acceptable - keeping it")
    
    # Import pyplot 
    import matplotlib.pyplot as plt
    
    # Basic configuration for terminal display
    plt.rcParams['figure.figsize'] = (8, 6)
    plt.rcParams['font.size'] = 10
    
    print(f"Final matplotlib backend: {matplotlib.get_backend()}")
    print("=== MATPLOTLIB SETUP COMPLETE ===")