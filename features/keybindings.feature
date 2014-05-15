Feature: Using keybindings
  
  Scenario: Using peep dired's keybindings
    Given I open dired buffer in the root directory
    When I run "peep-dired"
    Then key "<down>" should be mapped to "peep-dired-next-file"
    And key "C-n" should be mapped to "peep-dired-next-file"
    And key "<up>" should be mapped to "peep-dired-prev-file"
    And key "C-p" should be mapped to "peep-dired-prev-file"
    And key "<SPC>" should be mapped to "peep-dired-scroll-page-down"
    And key "C-<SPC>" should be mapped to "peep-dired-scroll-page-up"
    And key "<backspace>" should be mapped to "peep-dired-scroll-page-up"
    And I run "peep-dired"

