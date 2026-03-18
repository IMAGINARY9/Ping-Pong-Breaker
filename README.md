# Ping-Pong Breaker

A classic breakout-style arcade game written in **Racket**, featuring a paddled player, bouncing ball physics, destructible blocks, and enemy obstacles.

## Overview

Ping-Pong Breaker is an interactive game built with Racket's GUI framework. Navigate your paddle to keep the ball in play, destroy blocks with various hit points (1-3 hits), avoid dangerous enemies at the top of the screen, and don't let the ball fall off the bottom of the screen. Features smooth collision detection, physics-based ball reflection, and a grid-based block generation system.

**Genre**: Arcade / Breakout  
**Style**: Minimalist geometric design  
**Target Audience**: Educational (demonstrates game loop, collision detection, GUI programming)

## Features

- **Paddle Control**: Move left/right to track the bouncing ball
- **Physics-Based Ball**: Realistic ball reflection off surfaces (normal vector bouncing)
- **Block Destruction**: Blocks with variable durability (1-3 hit points) drop to lower durability colors when hit
- **Enemy Obstacles**: Three enemy blocks at top of screen that must be destroyed to win
- **Collision Detection**: Precise AABB-style collision between ball and all game entities
- **Game States**: Menu, Playing, Paused, Game Over, Victory
- **Customizable Frame Rate**: Easily switch between 60fps, 120fps, 250fps, or 1000fps
- **Unit-Tested Utilities**: Position math operations include comprehensive test cases

## Technology Stack

- **Language**: Racket (Scheme dialect)
- **Framework**: Racket GUI (`racket/gui`)
- **Testing**: RackUnit (embedded unit tests)
- **Platform**: Windows / macOS / Linux (anywhere Racket runs)

## Getting Started

### Prerequisites

- Racket 7.0+ ([Download](https://racket-lang.org/))
- Windows, macOS, or Linux

### Installation & Running

1. **Install Racket**:
   - Download from [racket-lang.org](https://racket-lang.org/)
   - Run installer and follow system prompts

2. **Run the Game**:
   ```bash
   racket ping-pong-breaker.rkt
   ```

   The game window will open automatically.

3. **Try Unit Tests** (optional):
   - Unit tests run automatically on startup
   - Look for test results in console output
   - All test predefined checks are performed

## How to Play

### Controls

| Action | Control |
|--------|---------|
| Move Paddle Left | `A` or `←` Arrow |
| Move Paddle Right | `D` or `→` Arrow |
| Pause Game | `P` or Pause Button |
| Menu Buttons | Click with mouse |

### Game Rules

1. **Objective**: Destroy all enemy blocks at the top while keeping the ball in play
2. **Scoring**: Each block destroyed increases your score
3. **Block HP**: Blocks show color based on remaining hit points:
   - 🟡 Light orange = 3 HP
   - 🟠 Medium orange = 2 HP  
   - 🔴 Dark red = 1 HP (destroyed on next hit)
4. **Lose Condition**: Ball falls past player paddle into the destroyer zone at bottom
5. **Win Condition**: All enemies at top are destroyed

### Game Modes

- **Play**: Start a new game
- **Restart**: Start fresh with new block layout
- **Pause**: Freeze gameplay temporarily
- **Continue**: Resume from paused state
- **Exit**: Return to menu
- **Quit**: Close the application

## Project Structure

```
ping-pong-breaker/
├── ping-pong-breaker.rkt       # Main game file (single-file project)
├── README.md                    # This file
└── [no additional assets]       # Pure Racket, no external dependencies
```

## Code Organization

The game is organized into logical sections:

### Constants & Configuration
- **Screen Size**: 512x768 pixels (32x48 cells @ 16px per cell)
- **Frame Rate**: 250fps default (configurable to 60/120/1000fps)
- **Colors & Brushes**: All visual styling defined at top (easily customizable)

### Core Data Structures
```racket
; posn : x y → Position in 2D space
(struct posn (x y))

; entity : id position size → Game entity (base for all objects)
(struct entity item (position size))

; drawable : entity form → Entity with visual form ("rect", "round-rect", "circle")
(struct drawable colorful (form))

; ball : drawable direction → Bouncing ball with trajectory
(struct ball drawable (direction))

; block : drawable hp → Destructible block with hit points
(struct block drawable (hp))

; button : drawable text font → Interactive UI element
(struct button colorful (text font))
```

### Major Systems

1. **Position Math** (lines 1-250)
   - `posn-add`, `posn-sum`, `posn-multiply`: Vector arithmetic
   - `posn-normalize`: Direction normalization
   - Tests verify all position calculations

2. **UI & Buttons** (lines 250-400)
   - `draw-button`: Renders button with text centering
   - `handle-button`: Processes button clicks and triggers game state changes

3. **Entities** (lines 400-600)
   - **Player Paddle**: Controlled entity at bottom, must stay on board
   - **Ball**: Self-contained physics with direction tracking
   - **Blocks**: Container holding list of destructible blocks with random HP
   - **Destroyer**: Lose zone—if ball collides here, game ends

4. **Physics & Collision** (lines 600-850)
   - `reflect-direction`: Ball bounce physics using normal vector reflection
   - `get-ball-cols`: Multi-surface collision detection
   - `check-hover`: AABB collision between point and entity

5. **Block System** (lines 850-1000)
   - `generate-blocks-list`: Creates random block grid
   - `generate-blocks-posns`: Calculates block positions within borders
   - `block-collide`: Handles block damage and destruction

6. **Game Display** (lines 1000-1200)
   - `draw-form`: Dispatcher for different shape renders
   - `draw-rectangle`, `draw-rounded-rectangle`, `draw-circle`
   - `paint-callback`: Main rendering function (called each frame)

7. **Game State** (lines 1200-1400)
   - `game-state`: Holds current game snapshot (player, ball, blocks, mode)
   - State transitions: Menu → Playing → Paused ↔ Continue → Game Over/Victory

8. **Main Game Loop** (lines 1400+)
   - `timer-callback`: Updates positions, checks collisions, advances frame
   - `handle-key`: Process keyboard input
   - `on-canvas-event`: Handle mouse clicks and pad movement
   - Full GUI window setup and initialization

## Development & Customization

### Changing Game Parameters

Edit the constants at the top of `ping-pong-breaker.rkt`:

```racket
; Screen size
(define WIDTH 512)
(define HEIGHT 768)

; Cell size (affects all proportions)
(define PIXEL 16)

; Frame rate (ms between updates; lower = faster)
;(define FRAME-RATE 17)  ;60fps
;(define FRAME-RATE 8)   ;120fps
(define FRAME-RATE 4)   ;250fps (default)
;(define FRAME-RATE 1)   ;1000fps

; Colors (modify RGB values to change appearance)
(define brush-ball (new brush% [color (make-object color% 60 60 125)]))
(define brush-player (new brush% [color (make-object color% 250 210 205)]))
(define brush-block1 (new brush% [color (make-object color% 250 210 170)]))
```

### Adding New Features

1. **New Block Type**: Add a variant in `block-collide` and colors in constants
2. **Power-ups**: Create new drawable types, adjust `paint-callback` to render them
3. **Sound Effects**: Use `(play-sound ...)` with Racket's sound library
4. **Difficulty Levels**: Modify `generate-blocks` count or FRAME-RATE dynamically
5. **Score System**: Add score tracking to `game-state` structure

### Running Unit Tests

Tests are embedded throughout the code using `(check-equal? ...)`:

```racket
; Run game normally—tests execute at startup
racket ping-pong-breaker.rkt

; To verify tests pass, check console for no error output
```

To view specific test failures:
```racket
; Uncomment test-engine at top of file for more verbose output
;(require test-engine/racket-tests)
```

## Known Limitations

- **Single File**: No modularization; large monolithic file makes extensive modification difficult
- **No Sound**: Visual-only experience (no audio feedback)
- **No Scoring Display**: Ball count and score not shown during gameplay
- **Fixed Difficulty**: Block count doesn't scale with difficulty
- **No Persistence**: No save/load game progress
- **Limited Enemies**: Only 3 fixed enemy blocks (redesign for dynamic count)

## Future Improvements

- [ ] Modularize into separate files (entities.rkt, physics.rkt, ui.rkt, etc.)
- [ ] Add sound effects (paddle hit, block break, game over)
- [ ] Implement score and lives display during gameplay
- [ ] Add difficulty progression (more blocks, faster ball after each level)
- [ ] Power-ups system (slow ball, expand paddle, multi-ball)
- [ ] Leaderboard/high score tracking
- [ ] Different game modes (endless, time attack, puzzle levels)
- [ ] Mobile/touch control support
- [ ] Configuration file for easy parameter tuning

## Architecture Notes

### Design Patterns Used

1. **Data-Driven Design**: All game objects are immutable structures; state changes create new versions
2. **Immediate Mode GUI**: Redraw entire screen each frame (no retained graphics state)
3. **Vector Math Abstraction**: Position operations isolated in dedicated functions with full test coverage
4. **Event-Driven Input**: Keyboard and mouse events trigger state updates asynchronously

### Performance Considerations

- Collision detection is O(n) where n = number of blocks; acceptable for ~100 blocks max
- Frame rate set to 250fps by default (update every 4ms); adjust FRAME-RATE constant if CPU-bound
- Position calculations use only floating-point math (no complex physics)
- Ball physics use basic reflection math (normalized surface normals)

## Credits

**Project Type**: Educational/Portfolio  
**Created**: As part of programming curriculum  
**Language**: Racket  
**Dependencies**: Only Racket standard library (racket/gui)

## License

[Specify if applicable—e.g., MIT, GPL, Public Domain]

---

**Version**: 1.0  
**Last Updated**: March 2026  
**Status**: Feature Complete (Educational)